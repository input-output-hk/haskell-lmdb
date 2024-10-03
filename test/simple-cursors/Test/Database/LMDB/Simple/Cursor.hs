module Test.Database.LMDB.Simple.Cursor (tests) where

import qualified Test.Database.LMDB.Simple.Cursor.Lockstep as LS
import           Test.Tasty

tests :: TestTree
tests = testGroup "Cursor" [
    LS.tests
  ]
