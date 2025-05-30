module Spec (testFolder, testRep, testDirKnitspeaks, parseFile) where

import Control.Monad (filterM, join)
import Data.Either (isRight)
import KSParser (ParseError, parseString)
import KSSyntax (Pattern)
import System.Directory.Extra (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- import Data.List (filter, intercalate)

testFolder :: IO ()
testFolder = do
  x <- testDirKnitspeaks
  mapM_ putStrLn x

testDirKnitspeaks :: IO [String]
testDirKnitspeaks = do
  f <- listDirectory "test"
  fs <- filterM (\x -> return $ takeExtension x == ".ks") f
  files <- mapM (return . ("test/" </>)) fs
  mapM parseFromFile files


parseFile :: String -> IO (String, Either ParseError Pattern)
parseFile f =
  do
    s <- readFile f
    return (f, parseString s)

parseFromFile :: String -> IO String
parseFromFile f =
  do
    s <- readFile f
    case parseString s of
      Left e -> return (join ["Error:", show f, ", message :", show e])
      Right _ -> return (join ["Parsed: ", show f])

testRep :: TestTree
testRep =
  testGroup
    "Test times parser"
    [ testCase "Zero times" $
        testExample "zero-times-test" False,
      testCase "Ten times" $
        testExample "times" True,
      testCase "Twice" $
        testExample "twice" True,
      testCase "Nested reps" $
        testExample "nested" True,
      testCase "Loops not allowed" $
        testExample "loops" False
    ]
  where
    testExample n res =
      do
        p <- readFile (join ["test/rep/", n, ".ks"])
        isRight (parseString p) @?= res
