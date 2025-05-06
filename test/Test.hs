import Spec (testFolder, testRep)
import StitchMapsTest (testStitchMapsKnitSpeaks)
import Test.Tasty

main :: IO ()
main =
  do
    --testStitchMapsKnitSpeaks
    testFolder
    defaultMain $
      localOption (mkTimeout 1000000) $
        testGroup
          "KnitSpeak - Main test Suite"
          [ testRep
          ]
{- 
    [] -> print (read "(Pattern [(Comment \"Hello\"), (Course (Row [1] None) [(Knittel (KInst K 1 (KArity 1) Nothing) )] \"\")])" :: Pattern)
 -}
{-
testGroup "Test Suite :"
[   example_parser_test
  , expression_tests
  , parser_tests
  , parse_statements
  , parse_error
  , interpreter_tests
  , expression_intepretation
  , expression_error
  , statement_interpretation
  , statement_errors
]
-}