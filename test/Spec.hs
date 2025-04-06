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
import Prelude

main :: IO ()
main = test

-- putStrLn "Test suite not yet implemented"

test :: IO ()
test = do
  x <- knitspeaks
  mapM_ putStrLn x

knitspeaks :: IO [String]
knitspeaks = do
  f <- listDirectory "test" -- getDirectoryContents "test"
  -- ks <- [y | y <- f,  takeExtension y == ".ks"] --findFilesWith (\x -> ".ks" `isSuffixOf` x)
  fs <- filterM (\x -> return $ takeExtension x == ".ks") f
  mapM parseFromFile fs

-- filterM (\x -> return (x /= "")) ks

-- getDirectoryContents "../KnitSpeakGenerator/knitspeaks" >>= print
parseFromFile :: String -> IO String
parseFromFile f =
  -- \| takeExtension f == ".ks" =
  do
    s <- readFile $ "test/" ++ f
    case parseString s of
      Left e -> return (join ["Error in file:", show f, ", error :", show e])
      Right p -> return (join ["Parsed: ", show f])

-- \| otherwise = return "" -- ("Error:" ++ show f ++ " has illegal file extention")

programStr :: Pattern -> String
programStr p = intercalate "\n" $ map show p