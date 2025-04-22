module Spec (testFolder, testRep, testKnitSpeaks) where

import Control.Monad (filterM, join)
import Data.Data (typeOf)
import Data.Either (isLeft, isRight, lefts, rights)
import Data.Either.Extra (fromLeft')
import KSParser (ParseError, parseString)
import KSSyntax (Pattern)
import System.Directory.Extra (createDirectoryIfMissing, listDirectory)
import System.FilePath (replaceDirectory, takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- import Data.List (filter, intercalate)

testFolder :: IO ()
testFolder = do
  x <- testDirKnitspeaks
  mapM_ putStrLn x

testKnitSpeaks :: IO ()
testKnitSpeaks = do
  results <- mapM (\n -> testDirStitchMapsKnitSpeaks (join ["../knitspeaks", show n, "/"])) ([1 .. 6] :: [Int])
  let errors = sum $ map fst results
      parsed = sum $ map snd results
      total = errors + parsed -- 10524
  putStrLn $ "Total patterns checked: " ++ show total
  putStrLn (show errors ++ " patterns did not parse")
  putStrLn (show parsed ++ " patterns parsed")

testDirKnitspeaks :: IO [String]
testDirKnitspeaks = do
  f <- listDirectory "test"
  fs <- filterM (\x -> return $ takeExtension x == ".ks") f
  files <- mapM (return . ("test/" </>)) fs
  mapM parseFromFile files

testDirStitchMapsKnitSpeaks :: String -> IO (Int, Int) -- [ParseError]
testDirStitchMapsKnitSpeaks dir = do
  f <- listDirectory dir
  fs <- filterM (\x -> return $ takeExtension x == ".ks") (reverse f)
  files <- mapM (return . (dir </>)) fs
  res <- mapM parseFile files
  let errors = filter (isLeft . snd) res
      l = length errors
      r = length $ filter (isRight . snd) res -- length $ rights res
  --writeErrors (map (\(x, y) -> (x, fromLeft' y)) errors)
  print (l, r)
  return (l, r)
  {- where
    writeErrors = mapM (writeError dir)
 -}
writeError :: String -> (String, ParseError) -> IO ()
writeError dir (f, pe) =
  do
    let errordir = dir </> "errors/"
    createDirectoryIfMissing False errordir
    writeFile (replaceDirectory f errordir) (show pe)

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
