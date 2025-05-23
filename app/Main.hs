--import KSSyntax
import KSParser             (parseString)
import Mirror               (mirror)
import Flip                 (flipPattern)
import Invert               (invert)
import Minimize             (minimize, minimize2, possibiblities)
import Unroll               (unroll, unrollRows)
import System.Environment   (getArgs)
import System.Exit          (die)
import Control.Monad        (join)
import Utils                
import Knittels 
import KSSyntax
import Data.List.Extra (lower)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] | f == "-h" || lower f == "--help" -> die usage 

    [file] -> do
        s <- readFile file
        case parseString s of
            Left e  -> putStrLn $ "*** Parse error: " ++ show e
            Right p -> print p

    [f, file]
        | f == "-p" || lower f == "--parse" -> do
            s <- readFile file
            case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right ast ->
                        case parseString $ show ast of
                            Left e     -> putStrLn $ "*** Parse error on generated KS: " ++ show e ++ "\n" ++ show ast
                            Right ast2 -> putStrLn $ join ["File: ", file, "\n", "ASTs are equal: ", show  (ast == ast2) , "\n", show ast2]

        | f == "-s" || lower f == "--mirror" -> do
            s <- readFile file
            case parseString s of
                Left e   -> putStrLn $ "*** Parse error: " ++ show e
                Right p  ->
                    case mirror p of
                        Left e -> putStrLn $ "*** Mirroring error: " ++ show e
                        Right m -> putStrLn $ join ["Pattern is symmetrical? ", show (p == m), "\n\n", show p, "\n\nInverted and reversed:\n", show m]

        | f == "-f" || lower f == "--flip" -> do
            s <- readFile file
            case parseString s of
                Left e   -> putStrLn $ "*** Parse error: " ++ show e
                Right p  -> 
                    let fp = flipPattern p in 
                        putStrLn $ join ["Pattern is equal? ", show (p == fp), "\n\n", show p, "\n\nFlipped:\n", show fp]

        | f == "-i" || lower f == "--invert" -> do
            s <- readFile file
            case parseString s of
                Left e   -> putStrLn $ "*** Parse error: " ++ show e
                Right p  -> putStrLn $ join ["\n", show p, "\n\nInverted:\n", show $ invert p]

        | f == "-m" || lower f == "--minimize" -> do
            s <- readFile file
            case parseString s of
                Left e   -> putStrLn $ "*** Parse error: " ++ show e
                Right p  -> 
                    let m = minimize p in 
                        putStrLn $ join ["Pattern is minimal? ", show (patternLength p == patternLength m), "\n\n", show p, "\n\nMinimized:\n", show m]
                        --"\n", show (map show (courseLengths p)), "\n", show (map show (courseLengths m))]
        | f == "-m2" -> do
            s <- readFile file
            case parseString s of
                Left e   -> putStrLn $ "*** Parse error: " ++ show e
                Right p  -> 
                        putStrLn $ join ["\n\n", show p, "\n", show (patternLength p), "\n", show (patternLength (unroll p)), "\n\nMinimized:\n", {- show (minimize2 p), -} "\n", show (possibiblities p)]
                        
        | f == "-u" || lower f == "--unroll" -> do
            s <- readFile file
            case parseString s of
                Left e   -> putStrLn $ "*** Parse error: " ++ show e
                Right p  -> putStrLn $ join [ show p, "\n\nUnrolled:\n", show $ unrollRows p]
        
        | f == "-ua" || lower f == "--unrollall" -> do
            s <- readFile file
            case parseString s of
                Left e   -> putStrLn $ "*** Parse error: " ++ show e
                Right p  -> putStrLn $ join [ show p, "\n\nUnrolled:\n", show $ unroll p]

    [f, file, file2] | f == "-c" || f == "--compare" -> do
        s <- readFile file
        sm <- readFile file2
        case parseString s of
            Left e   -> putStrLn $ "*** Parse error: " ++ show e
            Right p  ->
                case parseString sm of
                    Left e   -> putStrLn $ "*** Parse error: " ++ show e
                    Right p2 ->
                        case mirror p2 of
                            Left e -> putStrLn $ "*** Mirroring error: " ++ show e
                            Right m -> putStrLn $ join [
                                "Mirrored pattern is equal to second pattern? ",
                                show (removeComments m == removeComments p2),
                                "\n\n",
                                show p,
                                "\n\nMirrored:\n",
                                show m,
                                "\n\nMirrored example:\n",
                                show p2
                                ]

    [file, output]
        | head file == '-' -> die usage
        | otherwise        -> do
            s <- readFile file
            case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p ->
                    do  writeFile output $ show p
                        putStrLn $ "Pattern written to: " ++ show output

    _ ->
        die usage

usage :: String
usage =
  "Usage:\n\
  \ knitSpeak      PATTERN.ks            (parse only)\n\
  \ knitSpeak -s   PATTERN.ks            (mirror pattern)\n\
  \ knitSpeak -c   PATTERN.ks PATTERN.ks (mirror first pattern and compare with second)\n\
  \ knitSpeak -i   PATTERN.ks            (invert pattern)\n\
  \ knitSpeak -f   PATTERN.ks            (flip operations)\n\
  \ knitSpeak -m   PATTERN.ks            (minimize pattern)\n\
  \ knitSpeak -p   PATTERN.ks            (parse & assert output is equal)\n\
  \ knitSpeak      PATTERN.ks OUTPUT     (write to file)"
