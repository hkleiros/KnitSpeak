-- Abstract syntax definitions for KnitSpeak : https://stitch-maps.com/about/knitspeak/ 

module KSSyntax where
import Control.Monad ( join )
import Knittels (Knittel)
import Data.List (intercalate)

type Pattern = [Line]

data Line =
    Course Course  Instructions
    deriving (Eq, Read)

data Course =
      Round LineNums
    | Row LineNums Side
    deriving (Eq, Read)

type LineNums = [Integer]
data Side = R | W | None
    deriving (Eq, Read) -- Skal dette være spesifikt her eller en streng som vi må sjekke i intepreten? 

type Instructions = [Instruction]

-- NOTE: ønsker vi å ikke tillate mer enn en loop i lista og hindre at vi har loops med loops, reps med reps etc.?
data Instruction =
      Loop Instructions EndSts
    | Rep Instructions Times 
    | Knittel Knittel
    deriving (Eq, Read)

type EndSts = Integer
type Times = Integer

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
toRanges :: [Integer] -> [String]
toRanges = go Nothing
    where
    go :: Maybe (Integer, Integer) -> [Integer] -> [String]
    go Nothing (n:ns) = go (Just (n, n)) ns
    go (Just (start, latest)) (n:ns) | latest + 1 == n = go (Just (start, n)) ns
    go (Just (start, latest))    ns  | start == latest = show start : go Nothing ns
                                     | start + 1 == latest = show start : show latest : go Nothing ns
                                     | otherwise = (show start ++ "-" ++ show latest) : go Nothing ns
    go _ _ = []


instance Show Side where
    show None = ""
    show R = " (RS)"
    show W = " (WS)"

instance Show Instruction where
    show (Loop is 0)   = join ["*", intercalate ", " (map show is), ", repeat from *"]
    show (Loop is 1)   = join ["*", intercalate ", " (map show is), ", repeat from * to last st"]
    show (Loop is es)  = join ["*" ,intercalate ", " (map show is), ", repeat from * to last ", show es, " sts"]
    show (Rep is 2)    = join ["[", intercalate ", " (map show is), "] twice"]
    show (Rep is es)   = join ["[", intercalate ", " (map show is), "] ", show es, " times"]
    show (Knittel  k ) = show k

