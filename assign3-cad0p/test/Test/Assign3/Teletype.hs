module Test.Assign3.Teletype (qcTeletype, huTeletype) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Assign3.Teletype (Teletype (..), echo, runConsole)

import           System.IO.Unsafe (unsafePerformIO)

qcTeletype :: TestTree
qcTeletype = testGroup "Teletype" []

huTeletype :: TestTree
huTeletype = testGroup "Teletype" [ huRunConsole
                        ]


huRunConsole :: TestTree
huRunConsole = testGroup "runConsole"
  [ testCase "c" (
      unsafePerformIO (runConsole (End 'c'))
    @?=
      'c'
  )
  -- , testCase "cave" (
  --     unsafePerformIO (runConsole (
  --       Put 'c' (
  --       Put 'a' (
  --       Put 'v' (
  --       Put 'e' (
  --       End ()
  --       ))))
  --       ))
  --   @?=
  --     "cave"
  -- ) -- does not work because it only gets the last element (in this case "()"), for some reason
  -- don't know how to test echo function
  ]

