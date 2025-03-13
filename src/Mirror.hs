module Mirror (mirror, invert, stitchLength) where

import Knittels
    ( Knittel(..),
      KName(..),
      KArity(..), TBL (..) )
import KSSyntax (Pattern, Instruction(..), Instructions, Line(..) )

mirror :: Pattern -> Pattern
mirror = map sym
  where sym (Course c is) = Course c $ reverseInstructions (is, 0)

invert :: Pattern -> Pattern
invert = map inv
  where inv (Course c is) = Course c $ invertInstructions (is, 0)

invertInstructions :: (Instructions, Int) -> Instructions
invertInstructions (i: is, len) = invertInstruction (i, len) : invertInstructions (is, len + stitchLength i)  
invertInstructions ([], _)      = []

invertInstruction :: (Instruction, Int) -> Instruction
invertInstruction (Rep is times,  _) = Rep     (invertInstructions (is, 0)) times
invertInstruction (Loop  is _1, len) = Loop    (invertInstructions (is, 0)) len
invertInstruction (Knittel    k,  _) = Knittel (mirrorKnittel k)

reverseInstructions :: (Instructions, Int) -> Instructions
reverseInstructions (i: is, len) = reverseInstructions (is, len + stitchLength i) ++ [reverseInstruction (i, len)]
reverseInstructions ([], _)      = []

reverseInstruction :: (Instruction, Int) -> Instruction
reverseInstruction (Rep is times,  _) = Rep     (reverseInstructions (is, 0)) times
reverseInstruction (Loop  is _1, len) = Loop    (reverseInstructions (is, 0)) len
reverseInstruction (Knittel    k,  _) = Knittel (mirrorKnittel k)


stitchLength :: Instruction -> Int
stitchLength (Rep  is _) = sum (fmap stitchLength is)
stitchLength (Loop is _) = sum (fmap stitchLength is)
stitchLength (Knittel (KInst _ (KArity n ) _)) = n


mirrorKnittel :: Knittel -> Knittel
-- Decreases
mirrorKnittel (KInst (KNtog n) a (Just TBL)) = KInst (KNtogTwisted n) a Nothing
mirrorKnittel (KInst (KNtog 2) a t)          = KInst Ssk a t
mirrorKnittel (KInst (KNtog n ) a Nothing)   = KInst (KNtog n) a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt
mirrorKnittel (KInst Ssk a t)                = KInst (KNtog 2) a t
mirrorKnittel (KInst (PNtog n) a (Just TBL)) = KInst (PNtogTwisted n) a Nothing
mirrorKnittel (KInst (PNtog 2) a Nothing)    = KInst Ssp a Nothing
mirrorKnittel (KInst (PNtog 3) a Nothing)    = KInst Sssp a Nothing
mirrorKnittel (KInst (PNtog n) a Nothing)    = KInst (PNtog n) a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt
mirrorKnittel (KInst Ssp a t)                = KInst (PNtog 2) a t
mirrorKnittel (KInst Sssp a t)               = KInst (PNtog 3) a t


-- Increases
mirrorKnittel (KInst IncL a t)               = KInst IncR a t
mirrorKnittel (KInst IncR a t)               = KInst IncL a t
mirrorKnittel (KInst IncLp a t)              = KInst IncRp a t
mirrorKnittel (KInst IncRp a t)              = KInst IncLp a t

mirrorKnittel (KInst M1L a t)                = KInst M1R a t
mirrorKnittel (KInst M1R a t)                = KInst M1L a t
mirrorKnittel (KInst M1Lp a t)               = KInst M1Rp a t
mirrorKnittel (KInst M1Rp a t)               = KInst M1Lp a t

-- Cables 
mirrorKnittel (KInst One'1'1LT a t)          = KInst One'1'1RT a t
mirrorKnittel (KInst One'1'1RT a t)          = KInst One'1'1LT a t
mirrorKnittel (KInst (KNtogTwisted n) a t)   = KInst (KNtog n) a t
mirrorKnittel (KInst (PNtogTwisted n) a t)   = KInst (PNtog n) a t
mirrorKnittel (KInst (N'NLC n m) a t)        = KInst (N'NRC m n) a t
mirrorKnittel (KInst (N'NRC n m) a t)        = KInst (N'NLC m n) a t
mirrorKnittel (KInst (N'NLT  n m) a t)       = KInst (N'NRT m n) a t
mirrorKnittel (KInst (N'NRT  n m) a t)       = KInst (N'NLT m n) a t
mirrorKnittel (KInst (N'NRPT  n m) a t)      = KInst (N'NLPT m n) a t
mirrorKnittel (KInst (N'NLPT  n m) a t)      = KInst (N'NRPT m n) a t
mirrorKnittel (KInst (N'NLSC  n m) a t)      = KInst (N'NRSC m n) a t
mirrorKnittel (KInst (N'NRSC  n m) a t)      = KInst (N'NLSC m n) a t
mirrorKnittel (KInst One'1LSAC a t)          = KInst One'1RSAC a t
mirrorKnittel (KInst One'1RSAC a t)          = KInst One'1LSAC a t
mirrorKnittel (KInst One'1'1RPT  a t)        = KInst One'1'1LPT a t
mirrorKnittel (KInst One'1'1LPT a t)         = KInst One'1'1RPT a t

-- Beads ? KBL : KBR 



{- TODO: ... strikk alle knittels og se hva som er symmetrisk? -}

mirrorKnittel k = k
