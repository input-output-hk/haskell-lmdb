
{-| This module provides a read-only 'View' that is a snapshot of an LMDB
database at a single point in time. Because the view is unchanging, it can be
used within pure code. Behind the scenes, data is accessed from the underlying
LMDB memory map.

Each 'View' internally keeps open a read-only transaction in the LMDB
environment (consuming a slot in the lock table), so their use should be
minimized and generally short-lived. The transaction should be closed
automatically when the 'View' is garbage collected, but the timing is not
guaranteed.
-}

module Database.LMDB.Simple.View (
    -- * Creating
    View
  , newView
    -- * Operators
  , (!)
  , (!?)
    -- * Query
  , findWithDefault
  , lookup
  , member
  , notMember
  , null
  , size
    -- * Folds
  , foldViewWithKey
  , foldl
  , foldlWithKey
  , foldr
  , foldrWithKey
    -- * Conversion
  , elems
  , keys
  , toList
  ) where

import           Control.Concurrent.MVar (MVar, mkWeakMVar, newMVar, takeMVar,
                     tryReadMVar)
import           Control.Monad ((>=>))
import           Data.Maybe (fromMaybe, isJust)
import           Database.LMDB.Raw (MDB_dbi', MDB_txn, mdb_get', mdb_stat',
                     mdb_txn_begin, mdb_txn_commit, ms_entries)
import           Database.LMDB.Simple (Database)
import           Database.LMDB.Simple.Internal (Database (Db), Serialise,
                     forEachForward, forEachReverse, marshalIn, marshalOut,
                     peekMDBVal)
import           Foreign (alloca, nullPtr)
import           Prelude hiding (foldl, foldr, lookup, null)
import           System.IO.Unsafe (unsafePerformIO)

-- | A 'View' behaves much like a 'Data.Map.Map', except in the way it is
-- created. A @'View' k v@ maps keys @k@ to values @v@.
newtype View k v = View (MVar (MDB_txn, MDB_dbi'))

-- | Create and return a read-only 'View' for the given LMDB database.
-- Internally, a read-only transaction is opened and kept alive until the
-- 'View' is garbage collected.
newView :: Database k v -> IO (View k v)
newView (Db env dbi) = do
  txn <- mdb_txn_begin env Nothing True
  var <- newMVar (txn, dbi)
  _ <- mkWeakMVar var $ finalize var
  return (View var)

  where finalize :: MVar (MDB_txn, MDB_dbi') -> IO ()
        finalize = takeMVar >=> mdb_txn_commit . fst

{-# NOINLINE viewIO #-}
viewIO :: View k v -> ((MDB_txn, MDB_dbi') -> IO a) -> a
viewIO (View var) f = unsafePerformIO $
  tryReadMVar var >>= maybe (fail "finalized txn") (f >=> seq var . return)

-- | Is the view empty?
null :: View k v -> Bool
null view = viewIO view $ \(txn, dbi) -> do
  stat <- mdb_stat' txn dbi
  return (ms_entries stat == 0)

-- | The number of elements in the view.
size :: View k v -> Int
size view = viewIO view $ \(txn, dbi) -> do
  stat <- mdb_stat' txn dbi
  return (fromIntegral $ ms_entries stat)

-- | Is the key a member of the view? See also 'notMember'.
member :: Serialise k => k -> View k v -> Bool
member key view = viewIO view $ \(txn, dbi) ->
  marshalOut key (fmap isJust . mdb_get' txn dbi)

-- | Is the key not a member of the view? See also 'member'.
notMember :: Serialise k => k -> View k v -> Bool
notMember key view = not (member key view)

-- | Find the value at a key. Calls 'error' when the element can not be found.
(!) :: (Serialise k, Serialise v) => View k v -> k -> v
view ! key = fromMaybe notFoundError $ lookup key view
  where notFoundError = error "View.!: given key is not found in the database"
infixl 9 !

-- | Find the value at a key. Returns 'Nothing' when the element can not be found.
(!?) :: (Serialise k, Serialise v) => View k v -> k -> Maybe v
(!?) = flip lookup
infixl 9 !?

-- | Lookup the value at a key in the view.
--
-- The function will return the corresponding value as @('Just' value)@, or
-- 'Nothing' if the key isn't in the view.
lookup :: (Serialise k, Serialise v) => k -> View k v -> Maybe v
lookup key view = viewIO view $ \(txn, dbi) -> marshalOut key $
  mdb_get' txn dbi >=> maybe (return Nothing) (fmap Just . marshalIn)

-- | The expression @('findWithDefault' def k view)@ returns the value at key
-- @k@ or returns default value @def@ when the key is not in the view.
findWithDefault :: (Serialise k, Serialise v) => v -> k -> View k v -> v
findWithDefault def key = fromMaybe def . lookup key

-- | Fold the values in the view using the given right-associative binary
-- operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
foldr :: Serialise v => (v -> b -> b) -> b -> View k v -> b
foldr f z view = viewIO view $ \(txn, dbi) ->
  alloca $ \vptr ->
  forEachForward txn dbi nullPtr vptr z $ \rest ->
  f <$> peekMDBVal vptr <*> rest

-- | Fold the keys and values in the view using the given right-associative
-- binary operator, such that @'foldrWithKey' f z == 'Prelude.foldr'
-- ('uncurry' f) z . 'toList'@.
foldrWithKey :: (Serialise k, Serialise v)
             => (k -> v -> b -> b) -> b -> View k v -> b
foldrWithKey f z view = viewIO view $ \(txn, dbi) ->
  alloca $ \kptr ->
  alloca $ \vptr ->
  forEachForward txn dbi kptr vptr z $ \rest ->
  f <$> peekMDBVal kptr <*> peekMDBVal vptr <*> rest

-- | Fold the values in the view using the given left-associative binary
-- operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
foldl :: Serialise v => (a -> v -> a) -> a -> View k v -> a
foldl f z view = viewIO view $ \(txn, dbi) ->
  alloca $ \vptr ->
  forEachReverse txn dbi nullPtr vptr z $ \rest ->
  flip f <$> peekMDBVal vptr <*> rest

-- | Fold the keys and values in the view using the given left-associative
-- binary operator, such that @'foldlWithKey' f z == 'Prelude.foldl' (\\z'
-- (kx, x) -> f z' kx x) z . 'toList'@.
foldlWithKey :: (Serialise k, Serialise v)
             => (a -> k -> v -> a) -> a -> View k v -> a
foldlWithKey f z view = viewIO view $ \(txn, dbi) ->
  alloca $ \kptr ->
  alloca $ \vptr ->
  forEachReverse txn dbi kptr vptr z $ \rest ->
  (\k v a -> f a k v) <$> peekMDBVal kptr <*> peekMDBVal vptr <*> rest

-- | Fold the keys and values in the view using the given monoid.
foldViewWithKey :: (Monoid m, Serialise k, Serialise v)
                => (k -> v -> m) -> View k v -> m
foldViewWithKey f = foldrWithKey (\k v a -> f k v `mappend` a) mempty

-- | Return all elements of the view in the order of their keys.
elems :: Serialise v => View k v -> [v]
elems = foldr (:) []

-- | Return all keys of the view in the order they are stored in the
-- underlying LMDB database.
keys :: Serialise k => View k v -> [k]
keys view = viewIO view $ \(txn, dbi) ->
  alloca $ \kptr ->
  forEachForward txn dbi kptr nullPtr [] $ \rest ->
  (:) <$> peekMDBVal kptr <*> rest

-- | Convert the view to a list of key/value pairs.
toList :: (Serialise k, Serialise v) => View k v -> [(k, v)]
toList = foldrWithKey (\k v -> ((k, v) :)) []
