import Test.Tasty
-- import InterpreterTests 
-- import ParserTests
import Spec (testFolder, testRep)

main :: IO ()
main =
  do  testFolder 
      defaultMain $
        localOption (mkTimeout 1000000) 
        $ testGroup "Test Suite: " [
          testRep
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