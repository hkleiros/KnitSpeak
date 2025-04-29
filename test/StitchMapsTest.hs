module StitchMapsTest (testStitchMapsKnitSpeaks) where

import Control.Monad (filterM, join, mapM_)
import Data.Either (isLeft, isRight)
import Data.Either.Extra (fromLeft', fromRight')
import KSParser (ParseError, parseString)
import KSSyntax (Pattern)
import System.Directory.Extra (createDirectoryIfMissing, listDirectory)
import System.FilePath (replaceDirectory, takeExtension, (</>), takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Data.Set (fromList)
import Utils
import Minimize
import Invert
import Mirror
import Unroll
import Flip
import Data.List.Unique (isUnique)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.List.Extra (replace)

-- import Data.List (filter, intercalate)

testStitchMapsKnitSpeaks :: IO ()
testStitchMapsKnitSpeaks = do
  results <- mapM (\n -> testDirStitchMapsKnitSpeaks (join ["../knitspeaks", show n, "/"])) ([1 .. 6] :: [Int])
  let rnErrors = sum $ map fst results
      filesAndPatterns = snd  =<< results
      nrParsed = length filesAndPatterns
      total = rnErrors + nrParsed -- 10524
      patterns = map snd filesAndPatterns
      unique = fromList patterns
      nrUnique = length unique
  putStrLn $ "Total patterns checked: " ++ show total
  appendFile "results.tsv" "Name\tUnique?\t Length\tMinimized1 Length\tUnrolled length\tLossless?\tMirrored in Patterns?\tInverted in Patterns?\tFlipped in Patterns?\n"
  mapM_ (testPattern patterns) filesAndPatterns
  putStrLn (show rnErrors ++ " patterns did not parse")
  putStrLn (show nrParsed ++ " patterns parsed")
  putStrLn (show nrUnique ++ " unique patterns")
  putStrLn (show (nrParsed - nrUnique) ++ " copies")

  where
        testPattern :: [Pattern] ->  (String, Pattern) -> IO () 
        testPattern patterns (f, p) =
                      let --m = minimize p
                          m2 = minimize p
                          s = mirror p
                          in
                          case s of
                            Left e ->    appendFile "results.tsv" $ intercalate "\t" [ 
                              takeBaseName f,  
                              show (unique p), 
                              show (patternLength p),
                              show (patternLength (unroll p)), 
                              show ( patternLength m2), 
                              show (unroll p == unroll m2), 
                              "None",  
                              show (invert p `elem` patterns), 
                              show (flipPattern p `elem` patterns) ] ++ "\n"
                            Right mir -> appendFile "results.tsv" $ intercalate "\t" [
                              takeBaseName f,  
                              show (unique p), 
                              show (patternLength p),
                              show (patternLength (unroll p)), 
                              show ( patternLength m2),  
                              show (unroll p == unroll m2), 
                              show (mir `elem` patterns),  
                              show (invert p `elem` patterns), 
                              show (flipPattern p `elem` patterns) ] ++ "\n"
            where unique pa =
                    fromMaybe False (pa `isUnique` patterns)
testDirStitchMapsKnitSpeaks :: String -> IO (Int, [(String, Pattern)]) -- [ParseError]
testDirStitchMapsKnitSpeaks dir = do
  f <- listDirectory dir
  fs <- filterM (\x -> return $ takeExtension x == ".ks") (reverse f)
  files <- mapM (return . (dir </>)) fs
  res <- mapM parseFile files
  let errors = filter (isLeft . snd) res
      parsed = map (\(x,y) -> (x, fromRight' y)) (filter (isRight .snd) res)
      l = length errors
      r = length parsed
    -- _ <- writeErrors (map (\(x, y) -> (x, fromLeft' y)) errors)
  print (l, r)
  return (l, parsed)
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

