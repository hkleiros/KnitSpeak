module Spec (testFolder, testRep) where

import Control.Monad (filterM, join, sequence)
import Data.List (filter, intercalate)
import KSParser (parseString)
import KSSyntax
import System.Directory.Extra
  ( findFilesWith,
    getDirectoryContents,
    listDirectory,
  )
import System.FilePath (takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Data.Either (isLeft, isRight)


testFolder :: IO ()
testFolder = do
  x <- knitspeaks
  mapM_ putStrLn x

knitspeaks :: IO [String]
knitspeaks = do
  f <- listDirectory "test"
  fs <- filterM (\x -> return $ takeExtension x == ".ks") f
  mapM parseFromFile fs

-- getDirectoryContents "../KnitSpeakGenerator/knitspeaks" >>= print


-- NOTE: er det bedre å sjekke extention her eller i `knitspeaks`? Har egt ikke noe å si hvis vi ikke eksporterer metoden. 
parseFromFile :: String -> IO String
parseFromFile f =
  -- \| takeExtension f == ".ks" =
  do
    s <- readFile $ "test/" ++ f
    case parseString s of
      Left e -> return (join ["Error:", show f, ", message :", show e])
      Right p -> return (join ["Parsed: ", show f])

-- \| otherwise = return "" -- ("Error:" ++ show f ++ " has illegal file extention")

programStr :: Pattern -> String
programStr p = intercalate "\n" $ map show p

testRep :: TestTree
testRep = testGroup "Test times parser"
    [ testCase "Zero times" $
        testExample "zero-times-test" False
    , testCase "Ten times" $
        testExample "times" True
    , testCase "Twice" $
        testExample "twice" True
    , testCase "Nested reps" $
        testExample "nested" True
    , testCase "Loops not allowed" $
        testExample "loops" False
    ]
    where testExample n res  =
            do  p <- readFile (join ["test/rep/", n, ".ks"])
                isRight (parseString p) @?= res
