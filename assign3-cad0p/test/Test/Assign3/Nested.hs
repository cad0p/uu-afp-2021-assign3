module Test.Assign3.Nested (qcNested, huNested) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Assign3.Nested   (square1, square2, square3)

import           Prelude          hiding (foldr)

qcNested :: TestTree
qcNested = testGroup "Nested" []

huNested :: TestTree
huNested = testGroup "Nested" [ huEqSquare -- foldr
                        -- , huFoldr yFoldr
                        ]


huEqSquare :: TestTree
huEqSquare = testGroup "EqSquare"
  [ testCase "1 /= 2" ( assertBool "ok"(
      square1
    /=
      square2
  ))
  , testCase "2 /= 3" ( assertBool "ok"(
      square2
    /=
      square3
  ))
  , testCase "1 /= 3" ( assertBool "ok"(
      square1
    /=
      square3
  ))]

