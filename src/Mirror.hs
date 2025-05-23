module Mirror (mirror, stitchLength, mirrorKnittel) where

import KSSyntax (Course (..), Instruction (..), Instructions, Pattern (..), Line(..))
import Knittels
  ( KName (..),
    Knittel (..),
    TBL (..),
  )
import Utils (stitchLength)
import Data.Either 

newtype LoopError = LoopError String deriving (Eq, Read)
instance Show LoopError where
  show (LoopError m) = m 


countLoops :: Pattern -> Maybe LoopError
countLoops (Pattern p) =
  let n = lefts (map count p)
   in if null n -- > 1
        then Nothing
        else Just $ LoopError $ "more than one loop in one line cannot be mirrored. \n" ++  show (fst (head n)) ++ " loops in:  " ++ show (snd (head n))
  where
    count :: Course -> Either (Int, Line) Int
    count (Course l is _) =
      let n = countL is
       in if n > 1
            then
              Left (n, l)
            else Right n
    count _ = Right 0
    countL [] = 0
    countL (Loop _ _ : xs) = 1 + countL xs
    countL (_ : xs) = countL xs

mirror :: Pattern -> Either LoopError Pattern
mirror (Pattern p) = case countLoops (Pattern p) of
  Just e -> Left e
  Nothing -> Right $ Pattern $ map mir p
  where
    mir (Course l is c) = Course l (mirrorInstructions (is, 0)) c 
    mir c = c

-- Mirror functions
mirrorInstructions :: (Instructions, Int) -> Instructions
mirrorInstructions (i : is, len) = mirrorInstructions (is, len + stitchLength i) ++ [mirrorInstruction (i, len)]
mirrorInstructions ([], _) = []

mirrorInstruction :: (Instruction, Int) -> Instruction
mirrorInstruction (Rep is times, _) = Rep (mirrorInstructions (is, 0)) times
mirrorInstruction (Loop is _, len) = Loop (mirrorInstructions (is, 0)) len
mirrorInstruction (Knittel k, _) = Knittel (mirrorKnittel k)

mirrorKnittel :: Knittel -> Knittel
-- The operations which mirror images are:
mirrorKnittel (KInst (KNtog n) r a (Just TBL)) = KInst (KNtogTwisted n) r a Nothing
mirrorKnittel (KInst (KNtogTwisted n) r a Nothing)
  | n == 2 || n == 3 = KInst (KNtog n) r a (Just TBL)
  | otherwise = KInst (KNtog n) r a Nothing
mirrorKnittel (KInst (KNtog 2) r a t) = KInst Ssk r a t
mirrorKnittel (KInst (KNtog n) r a Nothing) = KInst (KNtog n) r a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt
mirrorKnittel (KInst (PNtog n) r a (Just TBL)) = KInst (PNtogTwisted n) r a Nothing
mirrorKnittel (KInst (PNtogTwisted n) r a Nothing) = KInst (PNtog n) r a (Just TBL)
mirrorKnittel (KInst (PNtog n) r a Nothing) = KInst (PNtog n) r a (Just TBL) -- NOTE: ikke ekte symmetrisk, men brukes ofte for samme visuelle effekt
mirrorKnittel (KInst k r a t) = KInst (mirrorKName k) r a t
-- 7

mirrorKName :: KName -> KName
mirrorKName Ssk = KNtog 2
mirrorKName Ssp = PNtog 2
mirrorKName Sssp = PNtog 3
mirrorKName (PNtog 2) = Ssp
mirrorKName (PNtog 3) = Sssp
mirrorKName (KNtogTwisted n) = KNtog n
mirrorKName (PNtogTwisted n) = PNtog n
-- Increases
mirrorKName IncL = IncR
mirrorKName IncR = IncL
mirrorKName IncLp = IncRp
mirrorKName IncRp = IncLp
mirrorKName M1L = M1R
mirrorKName M1R = M1L
mirrorKName M1Lp = M1Rp
mirrorKName M1Rp = M1Lp
-- Cables
mirrorKName One'1'1LT = One'1'1RT
mirrorKName One'1'1RT = One'1'1LT
mirrorKName (N'NLC n m) = N'NRC m n
mirrorKName (N'NRC n m) = N'NLC m n
mirrorKName (N'NLT n m) = N'NRT m n
mirrorKName (N'NRT n m) = N'NLT m n
mirrorKName (N'NRPC n m) = N'NLPC m n
mirrorKName (N'NLPC n m) = N'NRPC m n
mirrorKName (N'NRPT n m) = N'NLPT m n
mirrorKName (N'NLPT n m) = N'NRPT m n
mirrorKName (N'NLSC n m) = N'NRSC m n
mirrorKName (N'NRSC n m) = N'NLSC m n
mirrorKName One'1LSAC = One'1RSAC
mirrorKName One'1RSAC = One'1LSAC
mirrorKName One'1'1RPT = One'1'1LPT
mirrorKName One'1'1LPT = One'1'1RPT
mirrorKName (N'N'NLC n m l) = N'N'NRC l m n
mirrorKName (N'N'NRC n m l) = N'N'NLC l m n
mirrorKName (N'N'NLCC n m l) = N'N'NRCC l m n
mirrorKName (N'N'NRCC n m l) = N'N'NLCC l m n
mirrorKName (N'N'NLPC n m l) = N'N'NRPC l m n
mirrorKName (N'N'NRPC n m l) = N'N'NLPC l m n
-- Beads
mirrorKName KBL = KBR
mirrorKName KBR = KBL
-- Clusters
mirrorKName (SlN_kN_yo_psso 1 1) = P2so_yo_k1
mirrorKName P2so_yo_k1 = SlN_kN_yo_psso 1 1
mirrorKName Sl1_k1_yo_k1_psso = P3so_k1_yo_k1
mirrorKName P3so_k1_yo_k1 = Sl1_k1_yo_k1_psso
mirrorKName P3so_k1_yo_ssk = Sl1_p3so_k2tog_yo_k1
mirrorKName Sl1_p3so_k2tog_yo_k1 = P3so_k1_yo_ssk
mirrorKName (SlN_kN_yo_psso 1 n) = Yo_kN_pyo n
mirrorKName (Yo_kN_pyo n) = SlN_kN_yo_psso 1 n

-- Others
mirrorKName (N'NLeftThreadThru n m) = N'NRightThreadThru m n
mirrorKName (N'NRightThreadThru n m) = N'NLeftThreadThru m n
mirrorKName (N'NPurledLeftThreadThru n m) = N'NPurledRightThreadThru n m
mirrorKName (N'NPurledRightThreadThru n m) = N'NPurledLeftThreadThru n m
mirrorKName k = k
