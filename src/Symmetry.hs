module Symmetry () where

import Knittels

import KSSyntax ( Instruction(..), Instructions, Line(..), EndSts )
import qualified Data.Maybe


n, m :: InstructionNum
n = 0 -- Vi skal ikke kunne ha 0 som instructionNum så det kan være en plassholder? 
m = 0 -- Hvordan funker plassholdere her

-- NOTE: dele opp i de som trenger tall og de som ikke trenger tall ?
-- blir fortsatt et problem å generalisere dem :( 
symmetries ::   [(Knittel, Knittel)]
symmetries =    [
                  (KNtog 2, Ssk),               (Ssk, KNtog 2)
                , (IncL, IncR),                 (IncR, IncL)
                , (IncLp, IncRp),               (IncRp, IncLp)
                , (PNtog 2, Ssp),               (Ssp, PNtog 2)
                , (One'1'1LT, One'1'1RT),       (One'1'1RT, One'1'1LT)
                , (KNtog n, KNtogTwisted n),    (KNtogTwisted n, KNtog n)
                , (PNtog n, PNtogTwisted n),    (PNtogTwisted n, PNtog n)
                , (N'NLC n m, N'NRC m n),       (N'NRC n m, N'NLC m n) -- TODO: sjekk dette
                ]

symmetrical :: Line -> Line
symmetrical (Course c is) = Course c $ reverse $ reverseInstructions is 0


reverseInstructions :: Instructions -> Integer -> Instructions
-- TODO: how to reverse loop if there is rep in the end stitches? 
-- Plusse på en for hver ting og sende det med til reverseInstruction? 
--reverseInstructions (fs ++  [Loop _ _] ++  ls) = undefined
reverseInstructions (i: is) len =  reverseInstruction i len : reverseInstructions is len +(stitchLength i) 

reverseInstruction :: Instruction -> Instruction
reverseInstruction (Rep is times) = Rep (reverseInstructions is) times
reverseInstruction (Loop is  end) = undefined
reverseInstruction (Knittel    k) = Knittel $ symmetricalKnittel k

reverseLoop    :: Instructions -> EndSts -> Instruction
reverseLoop is = Loop (reverseInstructions is)

symmetricalKnittel :: Knittel -> Knittel
-- NOTE: lookup knittel in the table, match on the correct number etc swap with the snd and make number right
symmetricalKnittel  k = Data.Maybe.fromMaybe k (lookup k symmetries)
-- TODO: finn ut hvordan man håndterer plassholderne ?



-- TODO: generer dette samtidig som parseren?
stitchLength :: Knittel -> InstructionNum
stitchLength (K n) = n
stitchLength (P n) = n
stitchLength (PNtog n) = n
stitchLength (PNtogTwisted n) = n
stitchLength (KNtog n) = n
stitchLength (KNtogTwisted n) = n
stitchLength (N'NLC n m) = n + m 
stitchLength (N'NRC n m) = n + m 
stitchLength kn = Data.Maybe.fromMaybe 0 (lookup kn stitchLengths)

stitchLengths :: [(Knittel, Integer)]
stitchLengths =  [
        (Ssk, 2),
        (One'1'1LT  ,2),
        (One'1'1LPT ,2),
        (One'1'1RT  ,2),
        (One'1'1RPT ,2),
        (IncLp , 1),
        (IncR , 1),
        (IncL , 1),
        (IncRp , 1)
    ]