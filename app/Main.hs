module Main (main) where

import KSSyntax
import KSInterpreter        (execute)
import KSParser           (parseString)
import System.Exit        (die)
import System.Environment (getArgs)

run :: Pattern -> IO ()
run p =
  do let (out, res) = execute p
     mapM_ putStrLn out
     case res of
       Nothing -> return ()
       Just e  -> putStrLn ("*** Runtime error: " ++ show e)

main :: IO ()
main = do args <- getArgs
          case args of
            ["-i", file] -> do --TODO: fjern? 
              s <- readFile file
              run $ read s
            ["-p", file] -> do
              s <- readFile file
              case parseString s of
                Left e  -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> putStrLn $ show p
            [file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> run p
            _ ->
              die "Usage:\n\
                    \  knitSpeak -p PATTERN.ks    (parse only)"
                    {-\n\ 
                    \ knitSpeak -i PATTERN.ast    (interpret only)\n\
                    \  knitSpeak PATTERN.ks       (parse & interpret)"-}
