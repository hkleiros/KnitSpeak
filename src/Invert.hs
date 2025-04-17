module Invert (invert) where

import Knittels
    ( Knittel(..),
      KName(..),
      TBL(..), 
      YarnPlacement(..) )
import KSSyntax 
    ( Pattern(..), 
      Instruction(..), 
      Instructions, 
      Course(..) )
import Mirror (stitchLength)


invert :: Pattern -> Pattern
invert (Pattern p) = Pattern $ map fl p 
    where fl (Course l is c) = Course l (invertInstructions (is, 0)) c
          fl x = x 


invertInstructions :: (Instructions, Int) -> Instructions
invertInstructions (i: is, len) = invertInstruction (i, len) : invertInstructions (is, len + stitchLength i)
invertInstructions ([], _)      = []

invertInstruction :: (Instruction, Int) -> Instruction
invertInstruction (Rep is times,  _) = Rep     (invertInstructions (is, 0)) times
invertInstruction (Loop  is _1, len) = Loop    (invertInstructions (is, 0)) len
invertInstruction (Knittel    k,  _) = Knittel (invertKnittel   k)


invertKnittel :: Knittel -> Knittel
invertKnittel (KInst K r a t)            = KInst P r a t
invertKnittel (KInst P r a t)            = KInst K r a t
invertKnittel (KInst (Slip n Wyib) r a t)    = KInst (Slip n Wyif) r a t
invertKnittel (KInst (Slip n Wyif) r a t)    = KInst (Slip n Wyib) r a t
invertKnittel (KInst Knit r a t)             = KInst Purl r a t
invertKnittel (KInst Purl r a t)             = KInst Knit r a t
invertKnittel (KInst K1Below r a t)          = KInst P1Below r a t
invertKnittel (KInst P1Below r a t)          = KInst K1Below r a t

-- Decreases
invertKnittel (KInst (KNtog n) r a t)        = KInst (PNtog n) r a t
invertKnittel (KInst (PNtog n) r a t)        = KInst (KNtog n) r a t
invertKnittel (KInst Ssk r a t)              = KInst Ssp r a t
invertKnittel (KInst Ssp r a t)              = KInst Ssk r a t

invertKnittel (KInst Sssp r a _)             = KInst (KNtog 3) r a (Just TBL)
invertKnittel (KInst (KNtogTwisted n) r a t) = KInst (PNtogTwisted n) r a t

-- Increases
invertKnittel (KInst Kfb r a t)              = KInst Pfb r a t
invertKnittel (KInst Pfb r a t)              = KInst Kfb r a t

invertKnittel (KInst IncL r a t)             = KInst IncLp r a t
invertKnittel (KInst IncLp r a t)            = KInst IncL r a t
invertKnittel (KInst IncR r a t)             = KInst IncRp r a t
invertKnittel (KInst IncRp r a t)            = KInst IncR r a t

invertKnittel (KInst M1L r a t)              = KInst M1Lp r a t
invertKnittel (KInst M1Lp r a t)             = KInst M1L r a t
invertKnittel (KInst M1R r a t)              = KInst M1Rp r a t
invertKnittel (KInst M1Rp r a t)             = KInst M1R r a t

-- Cables 
{-No fully purled cables are defined :(-}


-- Beads ? KBL : KBR 



{- TODO: ... strikk alle knittels og se hva som er symmetrisk? -}

invertKnittel k = k
