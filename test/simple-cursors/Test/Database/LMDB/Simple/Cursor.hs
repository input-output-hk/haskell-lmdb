module Test.Database.LMDB.Simple.Cursor (tests) where

import           Test.Tasty

import qualified Test.Database.LMDB.Simple.Cursor.Lockstep as LS

tests :: TestTree
tests = testGroup "Cursor" [
    LS.tests
  ]
