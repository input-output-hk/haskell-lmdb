{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | FFI bindings for LMDB.
--
-- This module is only defines the essential bindings for LMDB without
-- abstractions on top.
module Database.LMDB.FFI (
    -- * Version Macros
    mdbVersionMajor, mdbVersionMinor, mdbVersionPatch
    -- * Environment flags
  , mdbFixedmap, mdbNosubdir, mdbNosync, mdbRdonly, mdbNometasync
  , mdbWritemap, mdbMapasync, mdbNotls, mdbNolock, mdbNordahead, mdbNomeminit
    -- * Database flags
  , mdbReversekey, mdbDupsort, mdbIntegerkey, mdbDupfixed, mdbIntegerdup
  , mdbReversedup, mdbCreate
    -- * Write flags
  , mdbNooverwrite, mdbNodupdata, mdbCurrent, mdbReserve
  , mdbAppend, mdbAppenddup, mdbMultiple
    -- * Cursor get operations
  , MDB_cursor_op (..)
  , mdbFirst, mdbFirstDup, mdbGetBoth, mdbGetBothRange, mdbGetCurrent
  , mdbGetMultiple, mdbLast, mdbLastDup, mdbNext, mdbNextDup, mdbNextMultiple
  , mdbNextNodup, mdbPrev, mdbPrevDup, mdbPrevNodup, mdbSet, mdbSetKey
  , mdbSetRange
    -- * Return codes
  , mdbSuccess, mdbKeyexist, mdbNotfound, mdbPageNotfound, mdbCorrupted, mdbPanic
  , mdbVersionMismatch, mdbInvalid, mdbMapFull, mdbDbsFull, mdbReadersFull
  , mdbTlsFull, mdbTxnFull, mdbCursorFull, mdbPageFull, mdbMapResized
  , mdbIncompatible, mdbBadRslot, mdbBadTxn, mdbBadValsize, mdbBadDbi
  , mdbLastErrcode
    -- * Data structures
  , MDB_val (..)
  , MDB_stat (..)
  , MDB_envinfo (..)
    -- ** Opaque
  , MDB_env
  , MDB_txn
  , MDB_cursor
    -- ** Newtypes (typedefs)
  , MDB_mode_t (..)
  , MDB_dbi (..)
    -- * Functions
  , mdb_version
  , mdb_strerror
  , mdb_env_create
  , mdb_env_open
  , mdb_env_copy
  , mdb_env_stat
  , mdb_env_info
  , mdb_env_sync
  , mdb_env_close
  , mdb_env_set_flags
  , mdb_env_get_flags
  , mdb_env_get_path
  , mdb_env_set_mapsize
  , mdb_env_set_maxreaders
  , mdb_env_get_maxreaders
  , mdb_env_set_maxdbs
  , mdb_env_get_maxkeysize
  , mdb_txn_begin
  , mdb_txn_commit
  , mdb_txn_abort
  , mdb_dbi_open
  , mdb_stat
  , mdb_dbi_flags
  , mdb_dbi_close
  , mdb_drop
  , mdb_set_compare
  , mdb_set_dupsort
  , mdb_cmp
  , mdb_dcmp
  , mdb_cmp'
  , mdb_dcmp'
  , mdb_get
  , mdb_put
  , mdb_del
  , mdb_get'
  , mdb_put'
  , mdb_del'
  , mdb_cursor_open
  , mdb_cursor_close
  , mdb_cursor_get
  , mdb_cursor_put
  , mdb_cursor_del
  , mdb_cursor_count
  , mdb_cursor_open'
  , mdb_cursor_close'
  , mdb_cursor_get'
  , mdb_cursor_put'
  , mdb_cursor_del'
  , mdb_cursor_count'
  , mdb_txn_reset
  , mdb_txn_renew
  , mdb_reader_list
  , mdb_reader_check
  , MDB_cmp_func, wrapCmpFn
  , MDB_msg_func, wrapMsgFunc
  ) where

#include <lmdb.h>

import Foreign
import Foreign.C

{-------------------------------------------------------------------------------
  Version Macros
--------------------------------------------------------------------------------}

foreign import capi unsafe "lmdb.h value MDB_VERSION_MAJOR" mdbVersionMajor :: CLong
foreign import capi unsafe "lmdb.h value MDB_VERSION_MINOR" mdbVersionMinor :: CLong
foreign import capi unsafe "lmdb.h value MDB_VERSION_PATCH" mdbVersionPatch :: CLong

{-------------------------------------------------------------------------------
  Environment flags
--------------------------------------------------------------------------------}

foreign import capi unsafe "lmdb.h value MDB_FIXEDMAP" mdbFixedmap :: CUInt
foreign import capi unsafe "lmdb.h value MDB_NOSUBDIR" mdbNosubdir :: CUInt
foreign import capi unsafe "lmdb.h value MDB_NOSYNC" mdbNosync :: CUInt
foreign import capi unsafe "lmdb.h value MDB_RDONLY" mdbRdonly :: CUInt
foreign import capi unsafe "lmdb.h value MDB_NOMETASYNC" mdbNometasync :: CUInt
foreign import capi unsafe "lmdb.h value MDB_WRITEMAP" mdbWritemap :: CUInt
foreign import capi unsafe "lmdb.h value MDB_MAPASYNC" mdbMapasync :: CUInt
foreign import capi unsafe "lmdb.h value MDB_NOTLS" mdbNotls :: CUInt
foreign import capi unsafe "lmdb.h value MDB_NOLOCK" mdbNolock :: CUInt
foreign import capi unsafe "lmdb.h value MDB_NORDAHEAD" mdbNordahead :: CUInt
foreign import capi unsafe "lmdb.h value MDB_NOMEMINIT" mdbNomeminit :: CUInt

{-------------------------------------------------------------------------------
  Database flags
--------------------------------------------------------------------------------}

foreign import capi unsafe "lmdb.h value MDB_REVERSEKEY" mdbReversekey :: CUInt
foreign import capi unsafe "lmdb.h value MDB_DUPSORT" mdbDupsort :: CUInt
foreign import capi unsafe "lmdb.h value MDB_INTEGERKEY" mdbIntegerkey :: CUInt
foreign import capi unsafe "lmdb.h value MDB_DUPFIXED" mdbDupfixed :: CUInt
foreign import capi unsafe "lmdb.h value MDB_INTEGERDUP" mdbIntegerdup :: CUInt
foreign import capi unsafe "lmdb.h value MDB_REVERSEDUP" mdbReversedup :: CUInt
foreign import capi unsafe "lmdb.h value MDB_CREATE" mdbCreate :: CUInt

{-------------------------------------------------------------------------------
  Write flags
--------------------------------------------------------------------------------}

foreign import capi unsafe "lmdb.h value MDB_NOOVERWRITE" mdbNooverwrite :: CUInt
foreign import capi unsafe "lmdb.h value MDB_NODUPDATA" mdbNodupdata :: CUInt
foreign import capi unsafe "lmdb.h value MDB_CURRENT" mdbCurrent :: CUInt
foreign import capi unsafe "lmdb.h value MDB_RESERVE" mdbReserve :: CUInt
foreign import capi unsafe "lmdb.h value MDB_APPEND" mdbAppend :: CUInt
foreign import capi unsafe "lmdb.h value MDB_APPENDDUP" mdbAppenddup :: CUInt
foreign import capi unsafe "lmdb.h value MDB_MULTIPLE" mdbMultiple :: CUInt

{-------------------------------------------------------------------------------
  Cursor get operations
-------------------------------------------------------------------------------}

newtype MDB_cursor_op = MDB_cursor_op ( #type MDB_cursor_op )
  deriving (Show, Eq)
#{enum MDB_cursor_op, MDB_cursor_op, mdbFirst = MDB_FIRST, mdbFirstDup = MDB_FIRST_DUP, mdbGetBoth = MDB_GET_BOTH, mdbGetBothRange = MDB_GET_BOTH_RANGE, mdbGetCurrent = MDB_GET_CURRENT, mdbGetMultiple = MDB_GET_MULTIPLE, mdbLast = MDB_LAST, mdbLastDup = MDB_LAST_DUP, mdbNext = MDB_NEXT, mdbNextDup = MDB_NEXT_DUP, mdbNextMultiple = MDB_NEXT_MULTIPLE, mdbNextNodup = MDB_NEXT_NODUP, mdbPrev = MDB_PREV, mdbPrevDup = MDB_PREV_DUP, mdbPrevNodup = MDB_PREV_NODUP, mdbSet = MDB_SET, mdbSetKey = MDB_SET_KEY, mdbSetRange = MDB_SET_RANGE}

instance Bounded MDB_cursor_op where
  minBound = mdbFirst
  maxBound = mdbSetRange

{-------------------------------------------------------------------------------
  Return codes
--------------------------------------------------------------------------------}

foreign import capi unsafe "lmdb.h value MDB_SUCCESS" mdbSuccess :: CInt
foreign import capi unsafe "lmdb.h value MDB_KEYEXIST" mdbKeyexist :: CInt
foreign import capi unsafe "lmdb.h value MDB_NOTFOUND" mdbNotfound :: CInt
foreign import capi unsafe "lmdb.h value MDB_PAGE_NOTFOUND" mdbPageNotfound :: CInt
foreign import capi unsafe "lmdb.h value MDB_CORRUPTED" mdbCorrupted :: CInt
foreign import capi unsafe "lmdb.h value MDB_PANIC" mdbPanic :: CInt
foreign import capi unsafe "lmdb.h value MDB_VERSION_MISMATCH" mdbVersionMismatch :: CInt
foreign import capi unsafe "lmdb.h value MDB_INVALID" mdbInvalid :: CInt
foreign import capi unsafe "lmdb.h value MDB_MAP_FULL" mdbMapFull :: CInt
foreign import capi unsafe "lmdb.h value MDB_DBS_FULL" mdbDbsFull :: CInt
foreign import capi unsafe "lmdb.h value MDB_READERS_FULL" mdbReadersFull :: CInt
foreign import capi unsafe "lmdb.h value MDB_TLS_FULL" mdbTlsFull :: CInt
foreign import capi unsafe "lmdb.h value MDB_TXN_FULL" mdbTxnFull :: CInt
foreign import capi unsafe "lmdb.h value MDB_CURSOR_FULL" mdbCursorFull :: CInt
foreign import capi unsafe "lmdb.h value MDB_PAGE_FULL" mdbPageFull :: CInt
foreign import capi unsafe "lmdb.h value MDB_MAP_RESIZED" mdbMapResized :: CInt
foreign import capi unsafe "lmdb.h value MDB_INCOMPATIBLE" mdbIncompatible :: CInt
foreign import capi unsafe "lmdb.h value MDB_BAD_RSLOT" mdbBadRslot :: CInt
foreign import capi unsafe "lmdb.h value MDB_BAD_TXN" mdbBadTxn :: CInt
foreign import capi unsafe "lmdb.h value MDB_BAD_VALSIZE" mdbBadValsize :: CInt
foreign import capi unsafe "lmdb.h value MDB_BAD_DBI" mdbBadDbi :: CInt
foreign import capi unsafe "lmdb.h value MDB_LAST_ERRCODE" mdbLastErrcode :: CInt

{-------------------------------------------------------------------------------
  Data structures
-------------------------------------------------------------------------------}

data {-# CTYPE "struct MDB_stat" #-} MDB_stat = MDB_stat {
      ms_psize :: {-# UNPACK #-} !CUInt
    , ms_depth :: {-# UNPACK #-} !CUInt
    , ms_branch_pages :: {-# UNPACK #-} !CSize
    , ms_leaf_pages :: {-# UNPACK #-} !CSize
    , ms_overflow_pages :: {-# UNPACK #-} !CSize
    , ms_entries :: {-# UNPACK #-} !CSize
    }
    deriving (Show, Eq)

instance Storable MDB_stat where
    sizeOf _ = #{size struct MDB_stat}
    alignment _ = #{alignment struct MDB_stat}
    peek p = do
        ms_psize <- #{peek struct MDB_stat, ms_psize} p
        ms_depth <- #{peek struct MDB_stat, ms_depth} p
        ms_branch_pages <- #{peek struct MDB_stat, ms_branch_pages} p
        ms_leaf_pages <- #{peek struct MDB_stat, ms_leaf_pages} p
        ms_overflow_pages <- #{peek struct MDB_stat, ms_overflow_pages} p
        ms_entries <- #{peek struct MDB_stat, ms_entries} p
        return $! MDB_stat
            { ms_psize
            , ms_depth
            , ms_branch_pages
            , ms_leaf_pages
            , ms_overflow_pages
            , ms_entries
            }
    poke p x = do
        #{poke struct MDB_stat, ms_psize} p (ms_psize x)
        #{poke struct MDB_stat, ms_depth} p (ms_depth x)
        #{poke struct MDB_stat, ms_branch_pages} p (ms_branch_pages x)
        #{poke struct MDB_stat, ms_leaf_pages} p (ms_leaf_pages x)
        #{poke struct MDB_stat, ms_overflow_pages} p (ms_overflow_pages x)
        #{poke struct MDB_stat, ms_entries} p (ms_entries x)

data  {-# CTYPE "struct MDB_envinfo" #-} MDB_envinfo = MDB_envinfo
    { me_mapaddr :: {-# UNPACK #-} !(Ptr ())
    , me_mapsize :: {-# UNPACK #-} !CSize
    , me_last_pgno :: {-# UNPACK #-} !CSize
    , me_last_txnid :: {-# UNPACK #-} !CSize
    , me_maxreaders :: {-# UNPACK #-} !CUInt
    , me_numreaders :: {-# UNPACK #-} !CUInt
    } deriving (Eq, Show)

instance Storable MDB_envinfo where
    sizeOf _ = #{size struct MDB_envinfo}
    alignment _ = #{alignment struct MDB_envinfo}
    peek p = do
        me_mapaddr <- #{peek struct MDB_envinfo, me_mapaddr} p
        me_mapsize <- #{peek struct MDB_envinfo, me_mapsize} p
        me_last_pgno <- #{peek struct MDB_envinfo, me_last_pgno} p
        me_last_txnid <- #{peek struct MDB_envinfo, me_last_txnid} p
        me_maxreaders <- #{peek struct MDB_envinfo, me_maxreaders} p
        me_numreaders <- #{peek struct MDB_envinfo, me_numreaders} p
        return $! MDB_envinfo
            { me_mapaddr
            , me_mapsize
            , me_last_pgno
            , me_last_txnid
            , me_maxreaders
            , me_numreaders
            }
    poke p x = do
        #{poke struct MDB_envinfo, me_mapaddr} p (me_mapaddr x)
        #{poke struct MDB_envinfo, me_mapsize} p (me_mapsize x)
        #{poke struct MDB_envinfo, me_last_pgno} p (me_last_pgno x)
        #{poke struct MDB_envinfo, me_last_txnid} p (me_last_txnid x)
        #{poke struct MDB_envinfo, me_maxreaders} p (me_maxreaders x)
        #{poke struct MDB_envinfo, me_numreaders} p (me_numreaders x)

-- | A value stored in the database. Be cautious; committing the
-- transaction that obtained a value should also invalidate it;
-- avoid capturing MDB_val in a lazy value. A safe interface
-- similar to STRef could be provided by another module.
data {-# CTYPE "struct MDB_val" #-} MDB_val = MDB_val {
      mv_size :: {-# UNPACK #-} !CSize
    , mv_data :: {-# UNPACK #-} !(Ptr ())
    }

instance Storable MDB_val where
  sizeOf _ = #{size struct MDB_val}
  alignment _ = #{alignment struct MDB_val}
  peek p = do
      mv_size <- #{peek struct MDB_val, mv_size} p
      mv_data <- #{peek struct MDB_val, mv_data} p
      pure $! MDB_val { mv_size, mv_data }
  poke p x = do
      #{poke struct MDB_val, mv_size} p (mv_size x)
      #{poke struct MDB_val, mv_data} p (mv_data x)

-- opaque types

data {-# CTYPE "struct MDB_env" #-} MDB_env

data {-# CTYPE "struct MDB_txn" #-} MDB_txn

data {-# CTYPE "struct MDB_cursor" #-} MDB_cursor

-- typedefs

newtype MDB_mode_t = MDB_mode_t ( #type mdb_mode_t )
  deriving (Show, Eq, Num, Storable)

newtype MDB_dbi = MDB_dbi ( #type MDB_dbi )
  deriving (Show, Eq, Num, Storable)

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

-- FFI
--  'safe': higher overhead, thread juggling, allows callbacks into Haskell
--  'unsafe': lower overhead, reduced concurrency, no callbacks into Haskell
foreign import capi unsafe "lmdb.h mdb_version" mdb_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CString
foreign import capi unsafe "lmdb.h mdb_strerror" mdb_strerror :: CInt -> CString

foreign import capi "lmdb.h mdb_env_create" mdb_env_create :: Ptr (Ptr MDB_env) -> IO CInt
foreign import capi "lmdb.h mdb_env_open" mdb_env_open :: Ptr MDB_env -> CString -> CUInt -> MDB_mode_t -> IO CInt
foreign import capi "lmdb.h mdb_env_copy" mdb_env_copy :: Ptr MDB_env -> CString -> IO CInt
foreign import capi "lmdb.h mdb_env_stat" mdb_env_stat :: Ptr MDB_env -> Ptr MDB_stat -> IO CInt
foreign import capi "lmdb.h mdb_env_info" mdb_env_info :: Ptr MDB_env -> Ptr MDB_envinfo -> IO CInt
foreign import capi "lmdb.h mdb_env_sync" mdb_env_sync :: Ptr MDB_env -> CInt -> IO CInt
foreign import capi "lmdb.h mdb_env_close" mdb_env_close :: Ptr MDB_env -> IO ()
foreign import capi "lmdb.h mdb_env_set_flags" mdb_env_set_flags :: Ptr MDB_env -> CUInt -> CInt -> IO CInt
foreign import capi unsafe "lmdb.h mdb_env_get_flags" mdb_env_get_flags :: Ptr MDB_env -> Ptr CUInt -> IO CInt
foreign import ccall unsafe "lmdb.h mdb_env_get_path" mdb_env_get_path :: Ptr MDB_env -> Ptr (Ptr CChar) -> IO CInt
foreign import capi "lmdb.h mdb_env_set_mapsize" mdb_env_set_mapsize :: Ptr MDB_env -> CSize -> IO CInt
foreign import capi "lmdb.h mdb_env_set_maxreaders" mdb_env_set_maxreaders :: Ptr MDB_env -> CUInt -> IO CInt
foreign import capi unsafe "lmdb.h mdb_env_get_maxreaders" mdb_env_get_maxreaders :: Ptr MDB_env -> Ptr CUInt -> IO CInt
foreign import capi "lmdb.h mdb_env_set_maxdbs" mdb_env_set_maxdbs :: Ptr MDB_env -> MDB_dbi -> IO CInt
foreign import capi unsafe "lmdb.h mdb_env_get_maxkeysize" mdb_env_get_maxkeysize :: Ptr MDB_env -> IO CInt

foreign import capi "lmdb.h mdb_txn_begin" mdb_txn_begin :: Ptr MDB_env -> Ptr MDB_txn -> CUInt -> Ptr (Ptr MDB_txn) -> IO CInt
foreign import capi "lmdb.h mdb_txn_commit" mdb_txn_commit :: Ptr MDB_txn -> IO CInt
foreign import capi "lmdb.h mdb_txn_abort" mdb_txn_abort :: Ptr MDB_txn -> IO ()

foreign import capi "lmdb.h mdb_dbi_open" mdb_dbi_open :: Ptr MDB_txn -> CString -> CUInt -> Ptr MDB_dbi -> IO CInt
foreign import capi "lmdb.h mdb_stat" mdb_stat :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_stat -> IO CInt
foreign import capi "lmdb.h mdb_dbi_flags" mdb_dbi_flags :: Ptr MDB_txn -> MDB_dbi -> Ptr CUInt -> IO CInt
foreign import capi "lmdb.h mdb_dbi_close" mdb_dbi_close :: Ptr MDB_env -> MDB_dbi -> IO ()
foreign import capi "lmdb.h mdb_drop" mdb_drop :: Ptr MDB_txn -> MDB_dbi -> CInt -> IO CInt

-- comparisons may only be configured for a 'safe' MDB_dbi.
foreign import capi "lmdb.h mdb_set_compare" mdb_set_compare :: Ptr MDB_txn -> MDB_dbi -> FunPtr MDB_cmp_func -> IO CInt
foreign import capi "lmdb.h mdb_set_dupsort" mdb_set_dupsort :: Ptr MDB_txn -> MDB_dbi -> FunPtr MDB_cmp_func -> IO CInt

foreign import capi safe "lmdb.h mdb_cmp" mdb_cmp :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import capi safe "lmdb.h mdb_dcmp" mdb_dcmp :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import capi unsafe "lmdb.h mdb_cmp" mdb_cmp' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import capi unsafe "lmdb.h mdb_dcmp" mdb_dcmp' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt

foreign import capi safe "lmdb.h mdb_get" mdb_get :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import capi safe "lmdb.h mdb_put" mdb_put :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
foreign import capi safe "lmdb.h mdb_del" mdb_del :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import capi unsafe "lmdb.h mdb_get" mdb_get' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import capi unsafe "lmdb.h mdb_put" mdb_put' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
foreign import capi unsafe "lmdb.h mdb_del" mdb_del' :: Ptr MDB_txn -> MDB_dbi -> Ptr MDB_val -> Ptr MDB_val -> IO CInt

-- I dislike LMDB's cursor interface: one 'get' function with 18 special cases.
-- Seems like it should be 18 functions.
foreign import capi safe "lmdb.h mdb_cursor_open" mdb_cursor_open :: Ptr MDB_txn -> MDB_dbi -> Ptr (Ptr MDB_cursor) -> IO CInt
foreign import capi safe "lmdb.h mdb_cursor_close" mdb_cursor_close :: Ptr MDB_cursor -> IO ()
foreign import capi safe "lmdb.h mdb_cursor_get" mdb_cursor_get :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> MDB_cursor_op -> IO CInt
foreign import capi safe "lmdb.h mdb_cursor_put" mdb_cursor_put :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
foreign import capi safe "lmdb.h mdb_cursor_del" mdb_cursor_del :: Ptr MDB_cursor -> CUInt -> IO CInt
foreign import capi safe "lmdb.h mdb_cursor_count" mdb_cursor_count :: Ptr MDB_cursor -> Ptr CSize -> IO CInt

foreign import capi unsafe "lmdb.h mdb_cursor_open" mdb_cursor_open' :: Ptr MDB_txn -> MDB_dbi -> Ptr (Ptr MDB_cursor) -> IO CInt
foreign import capi unsafe "lmdb.h mdb_cursor_close" mdb_cursor_close' :: Ptr MDB_cursor -> IO ()
foreign import capi unsafe "lmdb.h mdb_cursor_get" mdb_cursor_get' :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> MDB_cursor_op -> IO CInt
foreign import capi unsafe "lmdb.h mdb_cursor_put" mdb_cursor_put' :: Ptr MDB_cursor -> Ptr MDB_val -> Ptr MDB_val -> CUInt -> IO CInt
foreign import capi unsafe "lmdb.h mdb_cursor_del" mdb_cursor_del' :: Ptr MDB_cursor -> CUInt -> IO CInt
foreign import capi unsafe "lmdb.h mdb_cursor_count" mdb_cursor_count' :: Ptr MDB_cursor -> Ptr CSize -> IO CInt

foreign import capi unsafe "lmdb.h mdb_txn_reset" mdb_txn_reset :: Ptr MDB_txn -> IO ()
foreign import capi "lmdb.h mdb_txn_renew" mdb_txn_renew :: Ptr MDB_txn -> IO CInt

foreign import capi "lmdb.h mdb_reader_list" mdb_reader_list :: Ptr MDB_env -> FunPtr MDB_msg_func -> Ptr () -> IO CInt
foreign import capi "lmdb.h mdb_reader_check" mdb_reader_check :: Ptr MDB_env -> Ptr CInt -> IO CInt

-- | User-defined comparison functions for keys.
type MDB_cmp_func = Ptr MDB_val -> Ptr MDB_val -> IO CInt
foreign import ccall "wrapper"  wrapCmpFn :: MDB_cmp_func -> IO (FunPtr MDB_cmp_func)

-- callback function for reader list (used internally to this binding)
type MDB_msg_func = CString -> Ptr () -> IO CInt
foreign import ccall "wrapper" wrapMsgFunc :: MDB_msg_func -> IO (FunPtr MDB_msg_func)
