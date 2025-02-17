module Symmetry (symmetrical, invert, stitchLength) where

import Knittels
    ( Knittel(..),
      KName(..),
      KArity(..), TBL (..) )
import KSSyntax (Pattern, Instruction(..), Instructions, Line(..) )

symmetrical :: Pattern -> Pattern
symmetrical = map sym
  where sym (Course c is) = Course c $ reverseInstructions (is, 0)

invert :: Pattern -> Pattern
invert = map mir
  where mir (Course c is) = Course c $ invertInstructions (is, 0)

invertInstructions :: (Instructions, Integer) -> Instructions
invertInstructions (i: is, len) = invertInstruction (i, len) : invertInstructions (is, len + stitchLength i)  
invertInstructions ([], _)      = []

invertInstruction :: (Instruction, Integer) -> Instruction
invertInstruction (Rep is times,  _) = Rep     (invertInstructions (is, 0)) times
invertInstruction (Loop  is _1, len)  = Loop   (invertInstructions (is, 0)) len
invertInstruction (Knittel    k,  _) = Knittel (symmetricalKnittel   k)

reverseInstructions :: (Instructions, Integer) -> Instructions
reverseInstructions (i: is, len) = reverseInstructions (is, len + stitchLength i) ++ [reverseInstruction (i, len)]
reverseInstructions ([], _)      = []

reverseInstruction :: (Instruction, Integer) -> Instruction
reverseInstruction (Rep is times,  _) = Rep     (reverseInstructions (is, 0)) times
reverseInstruction (Loop  is _1, len) = Loop    (reverseInstructions (is, 0)) len
reverseInstruction (Knittel    k,  _) = Knittel (symmetricalKnittel   k)


stitchLength :: Instruction -> Integer
stitchLength (Rep  is _) = sum (fmap stitchLength is)
stitchLength (Loop is _) = sum (fmap stitchLength is)
stitchLength (Knittel (KInst _ (KArity n ) _)) = n


symmetricalKnittel :: Knittel -> Knittel
-- Decreases
symmetricalKnittel (KInst (KNtog 2) a t)          = KInst Ssk a t
symmetricalKnittel (KInst Ssk a t)                = KInst (KNtog 2) a t
symmetricalKnittel (KInst (PNtog 2) a Nothing)    = KInst Ssp a Nothing
symmetricalKnittel (KInst (PNtog 3) a Nothing)    = KInst Sssp a Nothing
symmetricalKnittel (KInst Ssp a t)                = KInst (PNtog 2) a t
symmetricalKnittel (KInst Sssp a t)               = KInst (PNtog 3) a t
symmetricalKnittel (KInst (PNtog n) a (Just TBL)) = KInst (PNtogTwisted n) a Nothing
symmetricalKnittel (KInst (PNtog n) a Nothing)    = KInst (PNtog n) a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt

symmetricalKnittel (KInst (KNtog n) a (Just TBL)) = KInst (KNtogTwisted n) a Nothing
symmetricalKnittel (KInst (KNtog n ) a Nothing)   = KInst (KNtog n) a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt

-- Increases
symmetricalKnittel (KInst IncL a t)               = KInst IncR a t
symmetricalKnittel (KInst IncR a t)               = KInst IncL a t
symmetricalKnittel (KInst IncLp a t)              = KInst IncRp a t
symmetricalKnittel (KInst IncRp a t)              = KInst IncLp a t

symmetricalKnittel (KInst M1L a t)                = KInst M1R a t
symmetricalKnittel (KInst M1R a t)                = KInst M1L a t
symmetricalKnittel (KInst M1Lp a t)               = KInst M1Rp a t
symmetricalKnittel (KInst M1Rp a t)               = KInst M1Lp a t

-- Cables 
symmetricalKnittel (KInst One'1'1LT a t)          = KInst One'1'1RT a t
symmetricalKnittel (KInst One'1'1RT a t)          = KInst One'1'1LT a t
symmetricalKnittel (KInst (KNtogTwisted n) a t)   = KInst (KNtog n) a t
symmetricalKnittel (KInst (PNtogTwisted n) a t)   = KInst (PNtog n) a t
symmetricalKnittel (KInst (N'NLC n m) a t)        = KInst (N'NRC m n) a t
symmetricalKnittel (KInst (N'NRC n m) a t)        = KInst (N'NLC m n) a t
symmetricalKnittel (KInst (N'NLT  n m) a t)       = KInst (N'NRT m n) a t
symmetricalKnittel (KInst (N'NRT  n m) a t)       = KInst (N'NLT m n) a t
symmetricalKnittel (KInst (N'NRPT  n m) a t)      = KInst (N'NLPT m n) a t
symmetricalKnittel (KInst (N'NLPT  n m) a t)      = KInst (N'NRPT m n) a t
symmetricalKnittel (KInst (N'NLSC  n m) a t)      = KInst (N'NRSC m n) a t
symmetricalKnittel (KInst (N'NRSC  n m) a t)      = KInst (N'NLSC m n) a t
symmetricalKnittel (KInst One'1LSAC a t)          = KInst One'1RSAC a t
symmetricalKnittel (KInst One'1RSAC a t)          = KInst One'1LSAC a t
symmetricalKnittel (KInst One'1'1RPT  a t)        = KInst One'1'1LPT a t
symmetricalKnittel (KInst One'1'1LPT a t)         = KInst One'1'1RPT a t

-- Beads ? KBL : KBR 



{- TODO: ... strikk alle knittels og se hva som er symmetrisk? -}

symmetricalKnittel k = k
