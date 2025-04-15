module Flip (flipPattern) where

import Knittels
    ( Knittel(..),
      KName(..),
      TBL (..), YarnPlacement (..) )
import KSSyntax (Pattern(..), Instruction(..), Instructions, Course(..) )
import Mirror (stitchLength)


flipPattern :: Pattern -> Pattern
flipPattern (Pattern p) = Pattern $ map fl p 
    where fl (Course l is c) = Course l (flipInstructions (is, 0)) c
          fl x = x 


flipInstructions :: (Instructions, Int) -> Instructions
flipInstructions (i: is, len) = flipInstruction (i, len) : flipInstructions (is, len + stitchLength i)
flipInstructions ([], _)      = []

flipInstruction :: (Instruction, Int) -> Instruction
flipInstruction (Rep is times,  _) = Rep     (flipInstructions (is, 0)) times
flipInstruction (Loop  is _1, len) = Loop    (flipInstructions (is, 0)) len
flipInstruction (Knittel    k,  _) = Knittel (flippedKnittel   k)


flippedKnittel :: Knittel -> Knittel
flippedKnittel (KInst K r a t)            = KInst P r a t
flippedKnittel (KInst P r a t)            = KInst K r a t
flippedKnittel (KInst (Slip n Wyib) r a t)    = KInst (Slip n Wyif) r a t
flippedKnittel (KInst (Slip n Wyif) r a t)    = KInst (Slip n Wyib) r a t
flippedKnittel (KInst Knit r a t)             = KInst Purl r a t
flippedKnittel (KInst Purl r a t)             = KInst Knit r a t
flippedKnittel (KInst K1Below r a t)          = KInst P1Below r a t
flippedKnittel (KInst P1Below r a t)          = KInst K1Below r a t

-- Decreases
flippedKnittel (KInst (KNtog n) r a t)        = KInst (PNtog n) r a t
flippedKnittel (KInst (PNtog n) r a t)        = KInst (KNtog n) r a t
flippedKnittel (KInst Ssk r a t)              = KInst Ssp r a t
flippedKnittel (KInst Ssp r a t)              = KInst Ssk r a t

flippedKnittel (KInst Sssp r a _)             = KInst (KNtog 3) r a (Just TBL)
flippedKnittel (KInst (KNtogTwisted n) r a t) = KInst (PNtogTwisted n) r a t

-- Increases
flippedKnittel (KInst Kfb r a t)              = KInst Pfb r a t
flippedKnittel (KInst Pfb r a t)              = KInst Kfb r a t

flippedKnittel (KInst IncL r a t)             = KInst IncLp r a t
flippedKnittel (KInst IncLp r a t)            = KInst IncL r a t
flippedKnittel (KInst IncR r a t)             = KInst IncRp r a t
flippedKnittel (KInst IncRp r a t)            = KInst IncR r a t

flippedKnittel (KInst M1L r a t)              = KInst M1Lp r a t
flippedKnittel (KInst M1Lp r a t)             = KInst M1L r a t
flippedKnittel (KInst M1R r a t)              = KInst M1Rp r a t
flippedKnittel (KInst M1Rp r a t)             = KInst M1R r a t

-- Cables 
{-No fully purled cables are defined :(-}


-- Beads ? KBL : KBR 



{- TODO: ... strikk alle knittels og se hva som er symmetrisk? -}

flippedKnittel k = k
