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

invert :: Pattern -> Pattern
invert (Pattern p) = Pattern $ map fl p
    where fl (Course l is c) = Course l (invertInstructions is) c
          fl x = x

invertInstructions :: Instructions -> Instructions
invertInstructions = map invertInstruction

invertInstruction :: Instruction -> Instruction
invertInstruction (Rep is times) = Rep     (invertInstructions is) times
invertInstruction (Loop    is e) = Loop    (invertInstructions is) e
invertInstruction (Knittel    k) = Knittel (invertKnittel   k)

invertKnittel :: Knittel -> Knittel
invertKnittel (KInst Sssp r a _)               = KInst (KNtog 3) r a (Just TBL)
invertKnittel (KInst (KNtog 3) r a (Just TBL)) = KInst Sssp r a Nothing
invertKnittel (KInst k r a t)                  = KInst (invertKName k) r a t

invertKName :: KName -> KName
invertKName K  =  P
invertKName P  =  K
invertKName (Slip n Wyib) = Slip n Wyif
invertKName (Slip n Wyif) =  Slip n Wyib
invertKName Knit          = Purl
invertKName Purl          = Knit
invertKName K1Below       = P1Below
invertKName P1Below       = K1Below

-- Decreases
invertKName (KNtog n) = PNtog n
invertKName (PNtog n) = KNtog n
invertKName Ssk       = Ssp
invertKName Ssp       = Ssk

invertKName (KNtogTwisted n) = PNtogTwisted n
invertKName (PNtogTwisted n) = KNtogTwisted n
invertKName Cddp             = Sl2_k1_p2sso
invertKName Sl2_k1_p2sso     = Cddp
invertKName CddTwisted       = CddpTwisted
invertKName CddpTwisted      = CddTwisted


-- Increases
invertKName Kfb = Pfb
invertKName Pfb = Kfb

invertKName  IncL  = IncLp
invertKName  IncLp = IncL
invertKName  IncR  = IncRp
invertKName  IncRp = IncR

invertKName M1L  = M1Lp
invertKName M1Lp = M1L
invertKName M1R  = M1Rp
invertKName M1Rp = M1R

-- Cables 
{-No fully purled cables are defined :(-}


-- Beads ? 
invertKName KBL = KBR
invertKName KBR = KBL

-- Clusters
invertKName (Yo_kN_pyo n) = Yo_pN_pyo n
invertKName (Yo_pN_pyo n) = Yo_kN_pyo n
invertKName (SlN_kN_psso n m) = SlN_pN_psso n m
invertKName (SlN_pN_psso n m) = SlN_kN_psso n m


-- Others 
invertKName k = k
