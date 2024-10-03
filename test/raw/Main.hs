{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use camelCase" -}

module Main (main) where

import           Control.Concurrent.Async
import qualified Database.LMDB.FFI        as FFI
import           Database.LMDB.Raw
import           Foreign
import           Foreign.C
import           System.IO.Temp
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "cardano-lmdb-test" [
      testCase "example_lowLevel" example_lowLevel
    , testCase "example_highLevel" example_highLevel
    ]

-- | Example for using low-level FFI bindings
example_lowLevel :: Assertion
example_lowLevel = withSystemTempDirectory "example_lowLevel" $ \dir -> do
    -- Open environment
    alloca $ \envPtrPtr -> do
      rc_create <- FFI.mdb_env_create envPtrPtr
      0 @=? rc_create
      envPtr <- peek envPtrPtr
      cstr_dir <- newCString dir
      rc_open <- FFI.mdb_env_open envPtr cstr_dir 0 (6 * 64 + 6 * 8)
      0 @=? rc_open
      -- start transaction and open database
      alloca $ \txnPtrPtr -> alloca $ \dbiPtr -> do
        rc_txn_begin <- FFI.mdb_txn_begin envPtr nullPtr 0 txnPtrPtr
        0 @=? rc_txn_begin
        txnPtr <- peek txnPtrPtr
        rc_db_open <- FFI.mdb_dbi_open txnPtr nullPtr 0 dbiPtr
        0 @=? rc_db_open
        dbi <- peek dbiPtr
        -- put
        alloca $ \mdb_key -> alloca $ \mdb_value -> do
          alloca $ \mv_data_key -> alloca $ \mv_data_value -> do
            poke mv_data_key (17 :: Word64)
            poke mdb_key (MDB_val { mv_size = 8, mv_data = castPtr mv_data_key })
            poke mv_data_value (42 :: Word64)
            poke mdb_value (MDB_val { mv_size = 8, mv_data = castPtr mv_data_value })
            rc_put <- FFI.mdb_put txnPtr dbi (castPtr mdb_key) (castPtr mdb_value) 0
            0 @=? rc_put
        -- get
        alloca $ \mdb_key -> alloca $ \(mdb_value :: Ptr MDB_val) -> do
          alloca $ \mv_data_key -> do
            poke mv_data_key (17 :: Word64)
            poke mdb_key (MDB_val { mv_size = 8, mv_data = castPtr mv_data_key })
            rc_get <- FFI.mdb_get txnPtr dbi (castPtr mdb_key) (castPtr mdb_value)
            0 @=? rc_get
            val <- peek mdb_value >>= peek . castPtr . mv_data
            (42 :: Word64) @=? val
        -- close database and commit transaction
        FFI.mdb_dbi_close envPtr dbi
        rc_commit <- FFI.mdb_txn_commit txnPtr
        0 @=? rc_commit
      -- 1 entry
      alloca $ \statPtr -> do
        rc_stat <- FFI.mdb_env_stat envPtr statPtr
        0 @=? rc_stat
        stat <- peek statPtr
        1 @=? ms_entries stat
      -- close environment
      FFI.mdb_env_close envPtr

-- | Example for using high(er)-level bindings
example_highLevel :: Assertion
example_highLevel = withSystemTempDirectory "example_highLevel" $ \dir -> do
    -- open environment
    env <- mdb_env_create
    mdb_env_open env dir []
    -- run transaction in bound thread
    flip withAsyncBound wait $ do
      -- start transaction and open database
      txn <- mdb_txn_begin env Nothing False
      dbi <- mdb_dbi_open txn Nothing []
      -- no entries
      stat1 <- mdb_stat txn dbi
      0 @=? ms_entries stat1
      -- put
      alloca $ \mv_data_key -> alloca $ \mv_data_value -> do
        poke mv_data_key (17 :: Word64)
        let mdb_key = MDB_val { mv_size = 8, mv_data = castPtr mv_data_key }
        poke mv_data_value (42 :: Word64)
        let mdb_value = MDB_val { mv_size = 8, mv_data = castPtr mv_data_value }
        success <- mdb_put (compileWriteFlags []) txn dbi mdb_key mdb_value
        True @=? success
      -- 1 entry
      stat2 <- mdb_stat txn dbi
      1 @=? ms_entries stat2
      -- get
      alloca $ \mv_data_key -> do
        poke mv_data_key (17 :: Word64)
        let mdb_key = MDB_val { mv_size = 8, mv_data = castPtr mv_data_key }
        mdb_get txn dbi mdb_key >>= \case
          Nothing -> undefined
          Just mdb_value -> do
            8 @=? mv_size mdb_value
            val <- peek (castPtr $ mv_data mdb_value)
            (42 :: Word64) @=? val
      -- still 1 entry
      stat3 <- mdb_stat txn dbi
      1 @=? ms_entries stat3
      -- close database and commit transaction
      mdb_dbi_close env dbi
      mdb_txn_commit txn
    -- 1 entry
    stat <- mdb_env_stat env
    1 @=? ms_entries stat
    -- close environment in bound thread
    flip withAsyncBound wait $ do
      mdb_env_close env
