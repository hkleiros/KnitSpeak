module Flip (flipPattern) where

import Knittels
    ( Knittel(..),
      KName(..),
      TBL (..), YarnPlacement (..) )
import KSSyntax (Pattern, Instruction(..), Instructions, Line(..) )
import Mirror (stitchLength)


flipPattern :: Pattern -> Pattern
flipPattern = map fl
    where fl (Course c is) = Course c $ flipInstructions (is, 0)


flipInstructions :: (Instructions, Int) -> Instructions
flipInstructions (i: is, len) = flipInstruction (i, len) : flipInstructions (is, len + stitchLength i)
flipInstructions ([], _)      = []

flipInstruction :: (Instruction, Int) -> Instruction
flipInstruction (Rep is times,  _) = Rep     (flipInstructions (is, 0)) times
flipInstruction (Loop  is _1, len) = Loop    (flipInstructions (is, 0)) len
flipInstruction (Knittel    k,  _) = Knittel (flippedKnittel   k)


flippedKnittel :: Knittel -> Knittel
flippedKnittel (KInst (K n) a t)            = KInst (P n) a t
flippedKnittel (KInst (P n) a t)            = KInst (K n) a t
flippedKnittel (KInst (Slip n Wyib) a t)    = KInst (Slip n Wyif) a t
flippedKnittel (KInst (Slip n Wyif) a t)    = KInst (Slip n Wyib) a t
flippedKnittel (KInst Knit a t)             = KInst Purl a t
flippedKnittel (KInst Purl a t)             = KInst Knit a t
flippedKnittel (KInst K1Below a t)          = KInst P1Below a t
flippedKnittel (KInst P1Below a t)          = KInst K1Below a t

-- Decreases
flippedKnittel (KInst (KNtog n) a t)        = KInst (PNtog n) a t
flippedKnittel (KInst (PNtog n) a t)        = KInst (KNtog n) a t
flippedKnittel (KInst Ssk a t)              = KInst Ssp a t
flippedKnittel (KInst Ssp a t)              = KInst Ssk a t

flippedKnittel (KInst Sssp a _)             = KInst (KNtog 3) a (Just TBL)
flippedKnittel (KInst (KNtogTwisted n) a t) = KInst (PNtogTwisted n) a t

-- Increases
flippedKnittel (KInst Kfb a t)              = KInst Pfb a t
flippedKnittel (KInst Pfb a t)              = KInst Kfb a t

flippedKnittel (KInst IncL a t)             = KInst IncLp a t
flippedKnittel (KInst IncLp a t)            = KInst IncL a t
flippedKnittel (KInst IncR a t)             = KInst IncRp a t
flippedKnittel (KInst IncRp a t)            = KInst IncR a t

flippedKnittel (KInst M1L a t)              = KInst M1Lp a t
flippedKnittel (KInst M1Lp a t)             = KInst M1L a t
flippedKnittel (KInst M1R a t)              = KInst M1Rp a t
flippedKnittel (KInst M1Rp a t)             = KInst M1R a t

-- Cables 
{-No fully purled cables are defined :(-}


-- Beads ? KBL : KBR 



{- TODO: ... strikk alle knittels og se hva som er symmetrisk? -}

flippedKnittel k = k
