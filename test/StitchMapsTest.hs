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
import Unroll
import Mirror
import Flip
import Data.List.Unique (isUnique, isRepeated, countElem)
import Data.Maybe (fromMaybe)
import Data.List (intercalate, nub, sortBy, sort, nubBy)
import Data.List.Extra (replace)


-- import Data.List (filter, intercalate)
resultsFile :: String
resultsFile = "../results6.tsv"

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
  --saveUnique patterns filesAndPatterns
  let header = intercalate "\t" [
        "Name",
        "Unique?",
        "Length",
        "Unrolled length",
        "Minimized1 Length",
        "Lossless?",
        "Mirror is inverse?",
        "Invert is inverse?",
        "Flip is inverse?",
        "Mirrored in Patterns?",
        "Inverted in Patterns?",
        "Flipped in Patterns?"
        ]++ "\n"
  appendFile resultsFile header
  mapM_ (testPattern patterns) filesAndPatterns
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
                          flipP = flipPattern p
                          invertP = invert p
                          in
                           case s of
                            Left e ->    appendFile resultsFile $ intercalate "\t" [
                              takeBaseName f,
                              show u,
                              show (patternLength p),
                              show (patternLength (unroll p)),
                              show (patternLength m2),
                              show (unroll p == unroll m2),
                              "None",
                              show (p == invert invertP),
                              show (p == flipPattern flipP),
                              "None",
                              show (invertP `elem` corpus),
                              show (flipP `elem` corpus) ] ++ "\n"
                            Right mir ->
                              case mirror mir of
                                Left e ->
                                    appendFile resultsFile $ intercalate "\t" [
                                    takeBaseName f,
                                    show u,
                                    show (patternLength p),
                                    show (patternLength (unroll p)),
                                    show ( patternLength m2),
                                    show (unroll p == unroll m2),
                                    "None",
                                    show (p == invert invertP),
                                    show (p == flipPattern flipP),
                                    show (mir `elem` corpus),
                                    show (invertP `elem` corpus),
                                    show (flipP `elem` corpus) ] ++ "\n"
                                Right mir2 ->
                                  appendFile resultsFile $ intercalate "\t" [
                                    takeBaseName f,
                                    show u,
                                    show (patternLength p),
                                    show (patternLength (unroll p)),
                                    show ( patternLength m2),
                                    show (unroll p == unroll m2),
                                    show (p == mir2),
                                    show (p == invert invertP),
                                    show (p == flipPattern flipP),
                                    show (mir `elem` corpus),
                                    show (invertP `elem` corpus),
                                    show (flipP `elem` corpus) ] ++ "\n"
            where unique  =
                    fromMaybe False (p `isUnique` corpus )

        saveUnique :: [Pattern] -> [(String, Pattern)] -> IO ()
        saveUnique corpus filesAndPatterns =
            let u = filter (repeated . snd) filesAndPatterns
                res = map (\(f,p) -> (p `countElem` corpus, p, f)) u
                in
                do  print $ length res
                    mapM_ (\(n, p, f) -> appendFile "number_copies.txt" (join [show n,"\t", f, "\n"])) (sort {- (nubBy (\(c,p,f) (c2,p2,f2)-> p == p2) -} res)
              where repeated p = fromMaybe False (p `isRepeated` corpus)


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
  _ <- writeErrors (map (\(x, y) -> (x, fromLeft' y)) errors)
  print (l, r)
  return (l, parsed)
  where
    writeErrors = mapM (writeError dir)

writeError :: String -> (String, ParseError) -> IO ()
writeError dir (f, pe) =
  do
    let errordir = dir </> "errors2/"
    createDirectoryIfMissing False errordir
    writeFile (replaceDirectory f errordir) (show pe)

parseFile :: String -> IO (String, Either ParseError Pattern)
parseFile f =
  do
    s <- readFile f
    return (f, parseString s)

