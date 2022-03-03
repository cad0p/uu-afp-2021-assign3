module Test.Assign3.Fix (qcFix, huFix) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Assign3.Fix      (foldr)

import           Prelude          hiding (foldr)

qcFix :: TestTree
qcFix = testGroup "Fix" []

huFix :: TestTree
huFix = testGroup "Fix" [ huFoldr ]


huFoldr :: TestTree
huFoldr = testGroup "foldr"
  [ ]

