module Main (main) where

import KSSyntax
import KSInterpreter        (execute)
import KSParser           (parseString)
import System.Exit        (die)
import System.Environment (getArgs)
import Data.List (intercalate)
import Control.Monad (join)


run :: Pattern -> IO ()
run p =
  do let (out, res) = execute p
     mapM_ putStrLn out
     case res of
       Nothing -> return ()
       Just e  -> putStrLn ("*** Runtime error: " ++ show e)

{- NOTE: kan denne endres til å ta en mappe, og kjøre alle programmer i den, 
se hvilke som får feil og hvilke som får like datastrukturer også printe stats. -}
main :: IO ()
main = do args <- getArgs
          case args of
            ["-i", file] -> do -- TODO: fjern? 
              s <- readFile file
              run $ read s
            ["-p", file] -> do
                s <- readFile file
                case parseString s of
                    Left e  -> putStrLn $ "*** Parse error: " ++ show e
                    Right p -> putStrLn $ programStr p
            [file] -> do
                s <- readFile file
                case parseString s of
                    Left e -> putStrLn $ "*** Parse error: " ++ show e
                    Right ast -> 
                            case parseString $ programStr ast of
                                Left e     -> putStrLn $ "*** Parse error on generated KS: " ++ show e ++ "\n" ++ show ast
                                Right ast2 -> putStrLn $ join ["File: ", file, "\n", "AST are equal: ", show  (ast == ast2) , "\n", programStr ast2]

            _ ->
                die "Usage:\n\
                    \  knitSpeak -p PATTERN.ks    (parse only)\n\
                    \ knitSpeak PATTERN.ks       (parse & interpret)"
                    {-\n\ 
                    \ knitSpeak -i PATTERN.ast    (interpret only)\n\
                    -}

programStr :: Pattern -> String 
programStr p = intercalate "\n" (map show p)