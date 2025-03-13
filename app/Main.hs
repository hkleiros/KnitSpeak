module Main (main) where

import KSSyntax
--import KSInterpreter        (execute)
import KSParser             (parseString)
import Mirror               (mirror, invert)
import Flip                 (flipPattern)
import Minimize             (minimize)
import System.Environment   (getArgs)
import System.Exit          (die)
import Control.Monad        (join)
import Data.List            (intercalate)

{-
run :: Pattern -> IO ()
run p =
  do let (out, res) = execute p
     mapM_ putStrLn out
     case res of
       Nothing -> return ()
       Just e  -> putStrLn ("*** Runtime error: " ++ show e)
-}

{- NOTE: kan denne også ta en mappe, og kjøre alle programmer i den, 
se hvilke som får feil og hvilke som får like datastrukturer også printe stats. -}
main :: IO ()
main = do args <- getArgs
          case args of
            {-["-i", file] -> do -- TODO: fjern? 
              s <- readFile file
              run $ read s -}
            
            ["-p", file] -> do
                s <- readFile file
                case parseString s of
                    Left e  -> putStrLn $ "*** Parse error: " ++ show e
                    Right p -> putStrLn $ programStr p
            
            ["-m", file] -> do
                s <- readFile file
                case parseString s of
                    Left e   -> putStrLn $ "*** Parse error: " ++ show e
                    Right p  -> putStrLn $ join ["Pattern is symmetrical? ", show (p == mirror p), "\n\n", programStr p, "\n\nInverted and reversed:\n", programStr $ mirror p]

            ["-c", file, file2] -> do
                s <- readFile file
                sm <- readFile file2
                case parseString s of
                    Left e   -> putStrLn $ "*** Parse error: " ++ show e
                    Right p  -> 
                        case parseString sm of
                            Left e   -> putStrLn $ "*** Parse error: " ++ show e
                            Right p2 -> putStrLn $ join ["Mirrored pattern is equal to second pattern? ", show (mirror p == p2), "\n\n", programStr p, "\n\nMirrored:\n", programStr $ mirror p, "\n\nMirrored example:\n", programStr p2]

            ["-i", file] -> do
                s <- readFile file
                case parseString s of
                    Left e   -> putStrLn $ "*** Parse error: " ++ show e
                    Right p  -> putStrLn $ join ["Pattern is equal? ", show (p == invert p), "\n\n", programStr p, "\n\nInverted:\n", programStr $ invert p]
            
            ["-f", file] -> do
                s <- readFile file
                case parseString s of
                    Left e   -> putStrLn $ "*** Parse error: " ++ show e
                    Right p  -> putStrLn $ join ["\n", programStr p, "\n\nFlipped:\n", programStr $ flipPattern p]
            
            ["-min", file] -> do
                s <- readFile file
                case parseString s of
                    Left e   -> putStrLn $ "*** Parse error: " ++ show e
                    Right p  -> putStrLn $ join ["Pattern is minimal? ", show (p == minimize p), "\n\n", programStr p, "\n\nMinimized:\n", programStr $ minimize p]

            [file] -> do
                s <- readFile file
                case parseString s of
                    Left e -> putStrLn $ "*** Parse error: " ++ show e
                    Right ast ->
                            case parseString $ programStr ast of
                                Left e     -> putStrLn $ "*** Parse error on generated KS: " ++ show e ++ "\n" ++ show ast
                                Right ast2 -> putStrLn $ join ["File: ", file, "\n", "ASTs are equal: ", show  (ast == ast2) , "\n", programStr ast2]

            _ ->
                die "Usage:\n\
                    \ knitSpeak -p PATTERN.ks    (parse only)\n\
                    \ knitSpeak -m PATTERN.ks    (mirror pattern)\n\
                    \ knitSpeak -c PATTERN.ks PATTERN.ks (mirror first pattern and compare with second)\n\
                    \ knitSpeak -i PATTERN.ks    (invert operations)\n\
                    \ knitSpeak -f PATTERN.ks    (flip pattern)\n\
                    \ knitSpeak    PATTERN.ks    (parse & assert show works propperly)\n\
                    \ knitSpeak -min PATTERN.ks  (minimize pattern)\n"
                    {-\n\ 
                    \ knitSpeak -i PATTERN.ast   (interpret only)\n\
                    -}

programStr :: Pattern -> String
programStr p = intercalate "\n" $ map show p