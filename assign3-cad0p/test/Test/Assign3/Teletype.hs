module Test.Assign3.Teletype (qcTeletype, huTeletype) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Assign3.Teletype (Teletype (..), echo, mockConsole, runConsole)

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
  -- related: Or we could mock user input and output for testing purposes; How?
  ]

huMockConsole :: TestTree
huMockConsole = testGroup "mockConsole"
  [ testCase "e" (
      mockConsole (Put 'e' (End ())) "def"
    @?=
      ((), "e")
  )
  -- ok, so now I understood how to mock user input, but is this the correct output? I'm not 100% sure
  ]

