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
import Data.List.Unique (isUnique, countElem)
import Data.Maybe (fromMaybe)
import Data.List (intercalate, nub, sortBy, sort)
import Data.List.Extra (replace)

-- import Data.List (filter, intercalate)

testStitchMapsKnitSpeaks :: IO ()
testStitchMapsKnitSpeaks = do
  results <- mapM (\n -> testDirStitchMapsKnitSpeaks (join ["../knitspeaks", show n, "/"])) ([1 .. 6] :: [Int])
  let rnErrors = sum $ map fst results
      filesAndPatterns = nub (snd  =<< results)
      nrParsed = length filesAndPatterns
      total = rnErrors + nrParsed -- 10524
      patterns = map snd filesAndPatterns
      unique = fromList patterns
      nrUnique = length unique
  putStrLn $ "Total patterns checked: " ++ show total
  saveUnique patterns filesAndPatterns
  --appendFile "results.tsv" "Name\tUnique?\t Length\tMinimized1 Length\tUnrolled length\tLossless?\tMirrored in Patterns?\tInverted in Patterns?\tFlipped in Patterns?\n"
  --mapM_ (testPattern patterns) filesAndPatterns
  putStrLn (show rnErrors ++ " patterns did not parse")
  putStrLn (show nrParsed ++ " patterns parsed")
  putStrLn (show nrUnique ++ " unique patterns")
  putStrLn (show (nrParsed - nrUnique) ++ " copies")

  where
        testPattern :: [Pattern] ->  (String, Pattern) -> IO ()
        testPattern corpus (f, p) =
                      let --m = minimize p
                          m2 = minimize p
                          s = mirror p
                          u = unique
                          in
                           case s of
                            Left e ->    appendFile "results.tsv" $ intercalate "\t" [
                              takeBaseName f,
                              show u,
                              show (patternLength p),
                              show (patternLength (unroll p)),
                              show (patternLength m2),
                              show (unroll p == unroll m2),
                              "None",
                              show (invert p `elem` corpus),
                              show (flipPattern p `elem` corpus) ] ++ "\n"
                            Right mir -> appendFile "results.tsv" $ intercalate "\t" [
                              takeBaseName f,
                              show u,
                              show (patternLength p),
                              show (patternLength (unroll p)),
                              show ( patternLength m2),
                              show (unroll p == unroll m2),
                              show (mir `elem` corpus),
                              show (invert p `elem` corpus),
                              show (flipPattern p `elem` corpus) ] ++ "\n" 
            where unique  =
                    fromMaybe False (p `isUnique` corpus )

        saveUnique :: [Pattern] -> [(String, Pattern)] -> IO ()
        saveUnique corpus filesAndPatterns=
            let u = filter (not . unique . snd) filesAndPatterns in
                mapM_ (\(n, p) -> appendFile "number_copies.txt" (join [show n, "\n"])) (sort (nub (map (\(f,p) -> (p `countElem` corpus, p)) u)))
              where unique p = fromMaybe False (p `isUnique` corpus)

testDirStitchMapsKnitSpeaks :: String -> IO (Int, [(String, Pattern)]) -- [ParseError]
testDirStitchMapsKnitSpeaks dir = do
  f <- listDirectory dir
  fs <- filterM (\x -> return $ takeExtension x == ".ks") (reverse f)
  files <- mapM (return . (dir </>)) fs
  res <- mapM parseFile files
  let errors = filter (isLeft . snd) res
      parsed = map (\(x,y) -> (takeBaseName x, fromRight' y)) (filter (isRight .snd) res)
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

