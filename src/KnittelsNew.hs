{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module KnittelsNew where
import Control.Monad ( join )
import Knittels (InstructionNum, YarnPlacement)
import Text.ParserCombinators.Parsec
import General
import Data.Maybe (isNothing)

data Knittel = KInst KName KArity deriving(Eq)

data KName  =
          K     InstructionNum
        | P     InstructionNum
        | Knit  -- NOTE: bør mulighens være en instruction fordi den omhandler hele raden
        | Purl  -- NOTE: samme som over
        | Slip  InstructionNum YarnPlacement
        | KNtog InstructionNum
        | BO InstructionNum

-- generated from `generate_parser.py`

        | CO
        | CtrDblInc
        | IncL
        | IncLp
        | IncR
        | IncRp
        | Kfb
        | Pfb
        | St
        | Yo
        | BunnyEarsBackDec
        | BunnyEarsBackYo
        | BunnyEarsDec
        | BunnyEarsYo
        | CddTwisted
        | Cddp
        | CddpTwisted
        | Ssk
        | Ssp
        | Sssp
        | KBL
        | KBR
        | PB
        | PBk
        | PBp
        | PBsl
        | SB
        | Cn
        | Sts
        | BrSl
        | DipSt
        | DropSt
        | MB
        | MK
        | WAndt

        | N_to_NInc InstructionNum InstructionNum
        | M1L
        | M1Lp
        | M1R
        | M1Rp
        | KNtogTwisted InstructionNum
        | PNtog InstructionNum
        | PNtogTwisted InstructionNum
        | Sl1_k2tog_psso
        | Sl2_k1_p2sso
        | Sl1Wb
        | P2so_yo_k1
        | P3so_k1_yo_k1
        | P3so_k1_yo_ssk
        | Sl1_k1_yo_k1_psso
        | SlN_kN_yo_psso InstructionNum InstructionNum
        | SlN_kN_psso InstructionNum InstructionNum
        | SlN_pN_psso InstructionNum InstructionNum
        | Sl1_p3so_k2tog_yo_k1
        | Yo_kN_pyo InstructionNum
        | Yo_pN_pyo InstructionNum
        | N_to_NGather InstructionNum InstructionNum
        | WrapNSts InstructionNum
        | N'NLeftThreadThru InstructionNum InstructionNum
        | N'NPurledLeftThreadThru InstructionNum InstructionNum
        | N'NRightThreadThru InstructionNum InstructionNum
        | N'NPurledRightThreadThru InstructionNum InstructionNum
        | N'NLC InstructionNum InstructionNum
        | N'NLPC InstructionNum InstructionNum
        | N'NLT InstructionNum InstructionNum
        | N'NLPT InstructionNum InstructionNum
        | N'NLSC InstructionNum InstructionNum
        | One'1LSAC
        | N'N'NLC InstructionNum InstructionNum InstructionNum
        | N'N'NLPC InstructionNum InstructionNum InstructionNum
        | One'1'1LT
        | One'1'1LPT
        | N'N'NLCC InstructionNum InstructionNum InstructionNum
        | N'NRC InstructionNum InstructionNum
        | N'NRPC InstructionNum InstructionNum
        | N'NRT InstructionNum InstructionNum
        | N'NRPT InstructionNum InstructionNum
        | N'NRSC InstructionNum InstructionNum
        | One'1RSAC
        | N'N'NRC InstructionNum InstructionNum InstructionNum
        | N'N'NRPC InstructionNum InstructionNum InstructionNum
        | One'1'1RT
        | One'1'1RPT
        | N'N'NRCC InstructionNum InstructionNum InstructionNum
        | K1Below
        | P1Below
    deriving (Eq, Show, Read)


data KArity = KArity [Int] (Maybe TBL)
        deriving(Show, Eq)


data TBL = TBL
        deriving(Show, Eq)

instance Show Knittel where
    show (KInst k (KArity x t))
        | t == Just TBL = join [show k, " tbl"]
        | isNothing t  = show k

prettyPrint :: Knittel -> String
prettyPrint (KInst k (KArity x m )) = join [show k, showTBL m]
{-
prettyPrint (KInst (K n)              (KArity x m)) = join ["k", show n, showTBL m]
prettyPrint (KInst (P n)              (KArity x m)) = join ["p", show n, showTBL m]
prettyPrint (KInst (KNtog n)          (KArity x m)) = join ["k", show n, "tog", showTBL m]
prettyPrint (KInst (N'N'NRC n1 n2 n3) (KArity x m)) = join [show n1, "/", show n2, "/", show n3, " ", "RC", showTBL m]

-- Bedre måte å håndtere tomme og ikke tomme lister?     
prettyPrint (KInst (BO (Just n))             (KArity x m)) = join ["BO ", show n, showTBL m]
prettyPrint (KInst (BO Nothing) (KArity x m)) = join ["BO", showTBL m]
-}



-- NOTE: Problem med dette er at vi legger til ekstra mellomrom etter hver ting. 
showTBL :: Maybe TBL -> String
showTBL (Just TBL)  = " tbl"
showTBL Nothing     = ""

tbl :: Parser (Maybe TBL)
tbl =   try (do skipSymbol "tbl"
                return (Just TBL))
        <|>
        return Nothing