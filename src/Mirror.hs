module Mirror (mirror, invert, stitchLength) where

import Knittels
    ( Knittel(..),
      KName(..),
      KArity(..), TBL (..) )
import KSSyntax (Pattern, Instruction(..), Instructions, Course(..) )

mirror :: Pattern -> Pattern
mirror = map sym
  where sym (Course c is) = Course c $ reverseInstructions (is, 0)
        sym c = c

invert :: Pattern -> Pattern
invert = map inv
  where inv (Course c is) = Course c $ invertInstructions (is, 0)
        inv c = c

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
stitchLength (Knittel (KInst _ _ (KArity n ) _)) = n


mirrorKnittel :: Knittel -> Knittel
-- Decreases
mirrorKnittel (KInst (KNtog n) r a (Just TBL)) = KInst (KNtogTwisted n) r a Nothing
mirrorKnittel (KInst (KNtog 2) r a t)          = KInst Ssk r a t
mirrorKnittel (KInst (KNtog n ) r a Nothing)   = KInst (KNtog n) r a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt
mirrorKnittel (KInst Ssk r a t)                = KInst (KNtog 2) r a t
mirrorKnittel (KInst (PNtog n) r a (Just TBL)) = KInst (PNtogTwisted n) r a Nothing
mirrorKnittel (KInst (PNtog 2) r a Nothing)    = KInst Ssp r a Nothing
mirrorKnittel (KInst (PNtog 3) r a Nothing)    = KInst Sssp r a Nothing
mirrorKnittel (KInst (PNtog n) r a Nothing)    = KInst (PNtog n) r a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt
mirrorKnittel (KInst Ssp r a t)                = KInst (PNtog 2) r a t
mirrorKnittel (KInst Sssp r a t)               = KInst (PNtog 3) r a t


-- Increases
mirrorKnittel (KInst IncL r a t)               = KInst IncR r a t
mirrorKnittel (KInst IncR r a t)               = KInst IncL r a t
mirrorKnittel (KInst IncLp r a t)              = KInst IncRp r a t
mirrorKnittel (KInst IncRp r a t)              = KInst IncLp r a t

mirrorKnittel (KInst M1L r a t)                = KInst M1R r a t
mirrorKnittel (KInst M1R r a t)                = KInst M1L r a t
mirrorKnittel (KInst M1Lp r a t)               = KInst M1Rp r a t
mirrorKnittel (KInst M1Rp r a t)               = KInst M1Lp r a t

-- Cables 
mirrorKnittel (KInst One'1'1LT r a t)          = KInst One'1'1RT r a t
mirrorKnittel (KInst One'1'1RT r a t)          = KInst One'1'1LT r a t
mirrorKnittel (KInst (KNtogTwisted n) r a t)   = KInst (KNtog n) r a t
mirrorKnittel (KInst (PNtogTwisted n) r a t)   = KInst (PNtog n) r a t
mirrorKnittel (KInst (N'NLC n m) r a t)        = KInst (N'NRC m n) r a t
mirrorKnittel (KInst (N'NRC n m) r a t)        = KInst (N'NLC m n) r a t
mirrorKnittel (KInst (N'NLT  n m) r a t)       = KInst (N'NRT m n) r a t
mirrorKnittel (KInst (N'NRT  n m) r a t)       = KInst (N'NLT m n) r a t
mirrorKnittel (KInst (N'NRPT  n m) r a t)      = KInst (N'NLPT m n) r a t
mirrorKnittel (KInst (N'NLPT  n m) r a t)      = KInst (N'NRPT m n) r a t
mirrorKnittel (KInst (N'NLSC  n m) r a t)      = KInst (N'NRSC m n) r a t
mirrorKnittel (KInst (N'NRSC  n m) r a t)      = KInst (N'NLSC m n) r a t
mirrorKnittel (KInst One'1LSAC r a t)          = KInst One'1RSAC r a t
mirrorKnittel (KInst One'1RSAC r a t)          = KInst One'1LSAC r a t
mirrorKnittel (KInst One'1'1RPT  r a t)        = KInst One'1'1LPT r a t
mirrorKnittel (KInst One'1'1LPT r a t)         = KInst One'1'1RPT r a t

-- Beads ? KBL : KBR 



{- TODO: ... strikk alle knittels og se hva som er symmetrisk? -}

mirrorKnittel k = k
