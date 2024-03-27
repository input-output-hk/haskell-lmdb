{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use camelCase" -}

module Main (main) where

import           Database.LMDB.FFI
import           Foreign
import           Foreign.C
import           System.IO.Temp
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "cardano-lmdb" [
      testCase "example_initClose" example_initClose
    ]

example_initClose :: Assertion
example_initClose = withSystemTempDirectory "example_initClose" $ \dir -> do
    alloca $ \envPtrPtr -> do
      rc_create <- mdb_env_create envPtrPtr
      0 @=? rc_create
      envPtr <- peek envPtrPtr
      cstr_dir <- newCString dir
      rc_open <- mdb_env_open envPtr cstr_dir 0 (6 * 64 + 6 * 8)
      0 @=? rc_open
      alloca $ \txnPtrPtr -> do
        rc_txn_begin <- mdb_txn_begin envPtr nullPtr 0 txnPtrPtr
        0 @=? rc_txn_begin
        txnPtr <- peek txnPtrPtr
        alloca $ \dbiPtr -> do
          rc_db_open <- mdb_dbi_open txnPtr nullPtr 0 dbiPtr
          0 @=? rc_db_open
          dbi <- peek dbiPtr
          alloca $ \mdb_key -> alloca $ \mdb_value -> do
            alloca $ \mv_data_key -> alloca $ \mv_data_value -> do
              poke mv_data_key (17 :: Word64)
              poke mdb_key (MDB_val { mv_size = 8, mv_data = castPtr mv_data_key })
              poke mv_data_value (42 :: Word64)
              poke mdb_value (MDB_val { mv_size = 8, mv_data = castPtr mv_data_value })
              rc_put <- mdb_put txnPtr dbi (castPtr mdb_key) (castPtr mdb_value) 0
              0 @=? rc_put
          alloca $ \mdb_key -> alloca $ \(mdb_value :: Ptr MDB_val) -> do
            alloca $ \mv_data_key -> do
              poke mv_data_key (17 :: Word64)
              poke mdb_key (MDB_val { mv_size = 8, mv_data = castPtr mv_data_key })
              rc_get <- mdb_get txnPtr dbi (castPtr mdb_key) (castPtr mdb_value)
              0 @=? rc_get
              val <- peek mdb_value >>= peek . castPtr . mv_data
              val @=? (42 :: Word64)
          mdb_dbi_close envPtr dbi
        rc_commit <- mdb_txn_commit txnPtr
        0 @=? rc_commit
        alloca $ \statPtr -> do
          rc_stat <- mdb_env_stat envPtr statPtr
          0 @=? rc_stat
          stat <- peek statPtr
          1 @=? ms_entries stat
      mdb_env_close envPtr
