-- Abstract syntax definitions for KnitSpeak : https://stitch-maps.com/about/knitspeak/ 

module KSSyntax (
        Pattern,
        Line(..),
        Course(..),
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

type Pattern = [Line]

data Line =
    Course Course Instructions
    deriving (Eq, Read)

data Course =
      Round LineNums
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
instance Show Line where
    show (Course c i) = join [unwords [ show c, intercalate ", " (map show i)], "."]


instance Show Course where
    show (Row    [n] side) = join ["Row ", show n, show side, ":"]
    show (Row    ln  side) = join ["Rows ", snillFunksjon (toRanges ln), show side, ":"]
    show (Round       [n]) = join ["Round ", show n, ":"]
    show (Round       ln ) = join ["Rounds ", snillFunksjon (toRanges ln) , ":"]


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
    show (Rep is 2)    = join ["[", intercalate ", " (map show is), "] twice"]
    show (Rep is es)   = join ["[", intercalate ", " (map show is), "] ", show es, " times"]
    show (Knittel  k ) = show k
    
endStitches :: EndSts -> String
endStitches n | n == 0 = ""
              | n == 1 = " to last st"
              | otherwise = " to last " ++ show n ++ " sts"

