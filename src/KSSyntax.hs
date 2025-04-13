-- Abstract syntax definitions for KnitSpeak : https://stitch-maps.com/about/knitspeak/ 

module KSSyntax (
        Pattern,
        Course(..),
        Line(..),
        LineNums,
        Instructions,
        Instruction(..),
        EndSts,
        Times,
        Side(..)
        )where
import Control.Monad ( join )
import Knittels (Knittel (KInst), KName (..))
import Data.List (intercalate)

type Pattern = [Course]

data Course =
      Course Line Instructions
    | MultilineRepeat String LineNums Times  
    | Comment String
    deriving (Eq, Read)

data Line =
      Round LineNums Side
    | Row LineNums Side
    deriving (Eq, Read)

type LineNums = [Int]

data Side = R | W | None
    deriving (Eq, Read)

type Instructions = [Instruction]

data Instruction =
      Loop Instructions EndSts
    | Rep Instructions Times
    | Knittel Knittel
    deriving (Eq, Read)

type EndSts = Int
type Times = Int

-- Definitions of show 
instance Show Course where
    show (Course l i) = join [unwords [ show l, intercalate ", " (map show i)], "."]
    show (MultilineRepeat r l t) = unwords ["Repeat", r, showLineNums l, showTimes t]
    show (Comment s) = join ["(", s, ")"]


instance Show Line where
    show (Row    ln  side) = join ["Rows ", showLineNums ln, show side, ":"]
    show (Round  ln  side) = join ["Rounds ", showLineNums ln, show side , ":"]


showLineNums :: [Int] -> String
showLineNums [n] = show n 
showLineNums ln = snillFunksjon $ toRanges ln

-- NOTE: forkort dette til noe hyggelig, akkurat nå vil 1-10 printe 1,2,3,4,5,6,7,8,9,10
-- lag en snill funksjon som forenkler ranges og putter and mellom nest siste og siste tall hvis det ikke er en range 
snillFunksjon :: [String] -> String
snillFunksjon []           = ""
snillFunksjon [s]          = s
snillFunksjon [s, t]       = unwords [s, "and", t]
snillFunksjon (s : str)    = s ++ ", " ++ snillFunksjon str

-- Takk til Luna som kan tenke når jeg ikke kan.
toRanges :: [Int] -> [String]
toRanges = go Nothing
    where
    go :: Maybe (Int, Int) -> [Int] -> [String]
    go Nothing (n:ns) = go (Just (n, n)) ns
    go (Just (start, latest)) (n:ns) | latest + 1 == n      = go (Just (start, n)) ns
    go (Just (start, latest))    ns  | start == latest      = show start : go Nothing ns
                                     | start + 1 == latest  = show start : show latest : go Nothing ns
                                     | otherwise            = (show start ++ "-" ++ show latest) : go Nothing ns
    go _ _ = []


instance Show Side where
    show None = ""
    show R = " (RS)"
    show W = " (WS)"

instance Show Instruction where
    show (Loop [Knittel (KInst Knit _ _ _)] es) = "Knit" ++ endStitches es
    show (Loop [Knittel (KInst Purl _ _ _)] es) = "Purl" ++ endStitches es
    show (Loop is es)  = join ["*" ,intercalate ", " (map show is), ", repeat from *", endStitches es]
    show (Rep is es)   = join ["[", intercalate ", " (map show is), "] ", showTimes es]
    show (Knittel  k ) = show k

showTimes :: Int -> String
showTimes 0 = ""
showTimes 2 = "twice"
showTimes n = show n ++ " times"
endStitches :: EndSts -> String
endStitches n | n == 0 = ""
              | n == 1 = " to last st"
              | otherwise = " to last " ++ show n ++ " sts"

