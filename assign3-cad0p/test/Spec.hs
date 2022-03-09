
import           Test.Tasty

import           Test.Assign3.Fix      (huFix, qcFix)
import           Test.Assign3.Nested   (huNested, qcNested)
import           Test.Assign3.Teletype (huTeletype, qcTeletype)



main :: IO ()
main = do
  defaultMain tests

tests       ::  TestTree
tests       =   testGroup "Tests"       [ properties, unitTests ]

properties  ::  TestTree
properties  =   testGroup "Properties"  [ qcProps ]

qcProps     ::  TestTree
qcProps     =   testGroup "QuickCheck"  [ qcFix
                                        , qcNested
                                        , qcTeletype ]



unitTests   ::  TestTree
unitTests   =   testGroup "Unit tests"  [ hUnit ]

hUnit       ::  TestTree
hUnit       =   testGroup "HUnit"       [ huFix
                                        , huNested
                                        , huTeletype ]

