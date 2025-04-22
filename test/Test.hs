-- import InterpreterTests
-- import ParserTests
import Spec (testFolder, testRep, testKnitSpeaks)
import Test.Tasty

main :: IO ()
main =
  do
    --testKnitSpeaks
    testFolder
    defaultMain $
      localOption (mkTimeout 1000000) $
        testGroup
          "KnitSpeak - Main test Suite"
          [ testRep
          ]

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