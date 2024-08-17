-- Abstract syntax definitions for KnitSpeak : https://stitch-maps.com/about/knitspeak/ 

module KSSyntax where
import Control.Monad ( join )

type Pattern = [Line]

data Line =
          Course Course  Instructions
    deriving (Eq, Read) -- TODO: endre show :) 

instance Show Line where
        show (Course c i) = join [unwords ["Course", show c, show i], "\n"]

data Course =
          Round LineNums
        | Row LineNums Side
        -- FIXME: trengs Rounds og Rows også? 
        deriving (Eq, Show, Read)

type LineNums = [Integer]

data Side = R | W | None
    deriving (Eq, Show, Read) -- Skal dette være spesifikt her eller en streng som vi må sjekke i intepreten? 


-- TODO: ikke tillate mer enn en loop i lista og hindre at vi har loops med loops, reps med reps etc.
-- Tror ikke vi kommer rundt det. 
type Instructions = [Instruction]

data Instruction =
          Loop Instructions EndSts
        | Rep Instructions Times -- FIXME: hva skal i lista? [Knittel], men funker ikke helt. 
        | Knittels [Knittel]
        | Knittel Knittel
    deriving (Eq, Show, Read)
type InstructionNum = Integer -- må sjekke om de er negative 🫠 er det like greit å bare ha Integers også sjekke at de ikke er 0? har vi lov å ha 0? Skal vi bare skippe da eller si feil? 

type EndSts = Integer
type Times = Integer

-- TODO: utvid 
data Knittel = -- riktig bruk av ordet knittel? Blir det slitsomt å definer alle 252 på denne måten? 
          K     InstructionNum
        | P     InstructionNum
        | Slip  InstructionNum YarnPlacement
        | BO    InstructionNum -- NOTE: vi definerer ikke avfellingsteknikk i knitspeak, men kan tenkes på til senere
        | KNtog Integer
        | Knit  -- NOTE: skal kanskje være en instruction fordi den omhandler hele raden
        | Purl  -- NOTE: samme som over
        | Yo
        | Kfb
        | Ssk
    deriving (Eq, Show, Read)

data YarnPlacement = Wyif | Wyib
    deriving (Eq, Show, Read)
