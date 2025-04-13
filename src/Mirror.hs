module Mirror (mirror, invert, stitchLength) where

import Knittels
    ( Knittel(..),
      KName(..),
      KArity(..),
      TBL (..) )
import KSSyntax (Pattern, Instruction(..), Instructions, Course(..) )

mirror :: Pattern -> Pattern
mirror = map sym
  where sym (Course c is) = Course c $ reverseInstructions (is, 0)
        sym c = c

invert :: Pattern -> Pattern
invert = map inv
  where inv (Course c is) = Course c $ invertInstructions (is, 0)
        inv c = c


-- Invert functions
invertInstructions :: (Instructions, Int) -> Instructions
invertInstructions (i: is, len) = invertInstruction (i, len) : invertInstructions (is, len + stitchLength i)
invertInstructions ([], _)      = []

invertInstruction :: (Instruction, Int) -> Instruction
invertInstruction (Rep is times,  _) = Rep     (invertInstructions (is, 0)) times
invertInstruction (Loop  is _1, len) = Loop    (invertInstructions (is, 0)) len
invertInstruction (Knittel    k,  _) = Knittel (mirrorKnittel k)

-- Reverse functions
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
-- The operations which mirror images are 
mirrorKnittel (KInst (KNtog n) r a (Just TBL)) = KInst (KNtogTwisted n) r a Nothing
mirrorKnittel (KInst (KNtogTwisted n) r a Nothing) = KInst (KNtog n) r a (Just TBL)
mirrorKnittel (KInst (KNtog 2) r a t)          = KInst Ssk r a t
mirrorKnittel (KInst (KNtog n ) r a Nothing)   = KInst (KNtog n) r a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt
mirrorKnittel (KInst (PNtog n) r a (Just TBL)) = KInst (PNtogTwisted n) r a Nothing
mirrorKnittel (KInst (PNtogTwisted n) r a Nothing) = KInst (PNtog n) r a (Just TBL)
mirrorKnittel (KInst (PNtog n) r a Nothing)    = KInst (PNtog n) r a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt

mirrorKnittel (KInst k r a t) = KInst (mirrorKName k) r a t

mirrorKName :: KName -> KName
mirrorKName Ssk = KNtog 2
mirrorKName (PNtog 2) =  Ssp 
mirrorKName (PNtog 3) =  Sssp
mirrorKName Ssp   =  PNtog 2
mirrorKName Sssp  =  PNtog 3

-- Increases
mirrorKName IncL  =  IncR
mirrorKName IncR  =  IncL
mirrorKName IncLp =  IncRp
mirrorKName IncRp =  IncLp
mirrorKName M1L  =  M1R
mirrorKName M1R  =  M1L
mirrorKName M1Lp =  M1Rp
mirrorKName M1Rp =  M1Lp

-- Cables 
mirrorKName  One'1'1LT           =  One'1'1RT
mirrorKName  One'1'1RT           =  One'1'1LT
mirrorKName  (KNtogTwisted n)    =  KNtog n
mirrorKName  (PNtogTwisted n)    =  PNtog n
mirrorKName  (N'NLC n m)         =  N'NRC m n
mirrorKName  (N'NRC n m)         =  N'NLC m n
mirrorKName  (N'NLT  n m)        =  N'NRT m n
mirrorKName  (N'NRT  n m)        =  N'NLT m n
mirrorKName  (N'NRPT  n m)       =  N'NLPT m n
mirrorKName  (N'NLPT  n m)       =  N'NRPT m n
mirrorKName  (N'NLSC  n m)       =  N'NRSC m n
mirrorKName  (N'NRSC  n m)       =  N'NLSC m n
mirrorKName  One'1LSAC           =  One'1RSAC
mirrorKName  One'1RSAC           =  One'1LSAC
mirrorKName  One'1'1RPT          =  One'1'1LPT
mirrorKName  One'1'1LPT          =  One'1'1RPT
mirrorKName  (N'N'NLC n m l)     =  N'N'NRC l m n
mirrorKName  (N'N'NRC n m l)     =  N'N'NLC l m n
mirrorKName  (N'N'NLCC n m l)    =  N'N'NRCC l m n
mirrorKName  (N'N'NRCC n m l)    =  N'N'NRCC l m n
mirrorKName  (N'N'NLPC n m l)    =  N'N'NRPC l m n
mirrorKName  (N'N'NRPC n m l)    =  N'N'NLPC l m n

-- Beads 
mirrorKName  KBL = KBR
mirrorKName  KBR = KBL

-- Clusters 
mirrorKName  (SlN_kN_yo_psso 1 1)  =  P2so_yo_k1
mirrorKName  P2so_yo_k1            =  SlN_kN_yo_psso 1 1
-- mirrorKnittel (KInst (SlN_kN_psso 1 2) r a t) = KInst  r a t 
mirrorKName  Sl1_k1_yo_k1_psso  =  P3so_k1_yo_k1
mirrorKName  P3so_k1_yo_k1      = Sl1_k1_yo_k1_psso
-- mirrorKnittel (KInst _ r a t)


{- TODO: ... strikk alle knittels og se hva som er symmetrisk? -}

mirrorKName k = k
