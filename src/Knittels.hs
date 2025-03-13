{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Knittels (Knittel(..), KName(..), KArity(..), TBL(..), InstructionNum, YarnPlacement(..)) where
import Control.Monad (join)
import Data.Maybe (isNothing)

data Knittel = KInst KName KArity (Maybe TBL) deriving (Eq, Read)

instance Show Knittel where
    show (KInst k (KArity _) t)
        | t == Just TBL = join [show k, " tbl"]
        | isNothing t   = show k

newtype KArity = KArity Int
        deriving(Show, Eq, Read)

data TBL = TBL
        deriving(Show, Eq, Read)

type InstructionNum = Int 

data YarnPlacement = Wyif | Wyib
    deriving (Eq, Read)

instance Show YarnPlacement where
    show Wyif = " wyif"
    show Wyib = " wyib"


data KName = -- riktig bruk av ordet knittel? Blir det slitsomt å definer alle 252 på denne måten? 
          K     InstructionNum
        | P     InstructionNum
        | Knit  -- NOTE: bør mulighens være en instruction fordi den omhandler hele raden
        | Purl  -- NOTE: samme som over
        | Slip  InstructionNum YarnPlacement
        | KNtog InstructionNum
        | BO    (Maybe InstructionNum)
        | CO    (Maybe InstructionNum)

-- generated from `generate_parser.py`

        | CtrDblInc
        | IncL
        | IncLp
        | IncR
        | IncRp
        | Kfb
        | Pfb
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
    deriving (Eq, Read)

instance Show KName where
    show (K n)                            = join ["k", show n]
    show (P n)                            = join ["p", show n]
    show Knit                             = "Knit"
    show Purl                             = "Purl"
    show (Slip n yp)                      = join ["sl", show n, show yp]
    show (KNtog n1)                       = join ["k", show n1, "tog"]
    show (BO (Just n))                    = join ["BO ", show n, " sts"]
    show (BO Nothing)                     = "BO"
    show (CO (Just n))                    = join ["CO", show n, " sts"]
    show (CO Nothing)                     = "CO"

    show CtrDblInc                        = "ctr dbl inc"
    show IncL                             = "incL"
    show IncLp                            = "incLp"
    show IncR                             = "incR"
    show IncRp                            = "incRp"
    show Kfb                              = "kfb"
    show Pfb                              = "pfb"
    show Yo                               = "yo"
    show BunnyEarsBackDec                 = "bunny ears back dec"
    show BunnyEarsBackYo                  = "bunny ears back yo"
    show BunnyEarsDec                     = "bunny ears dec"
    show BunnyEarsYo                      = "bunny ears yo"
    show CddTwisted                       = "cdd twisted"
    show Cddp                             = "cddp"
    show CddpTwisted                      = "cddp twisted"
    show Ssk                              = "ssk"
    show Ssp                              = "ssp"
    show Sssp                             = "sssp"
    show KBL                              = "KBL"
    show KBR                              = "KBR"
    show PB                               = "PB"
    show PBk                              = "PBk"
    show PBp                              = "PBp"
    show PBsl                             = "PBsl"
    show SB                               = "SB"
    show BrSl                             = "brSl"
    show DipSt                            = "dip st"
    show DropSt                           = "drop st"
    show MB                               = "MB"
    show MK                               = "MK"
    show WAndt                            = "w&t"

    show (N_to_NInc n1 n2)                = join [show n1, "-", "to", "-", show n2, " ", "inc"]
    show M1L                              = "M1L"
    show M1Lp                             = "M1Lp"
    show M1R                              = "M1R"
    show M1Rp                             = "M1Rp"
    show (KNtogTwisted n1)                = join ["k", show n1, "tog", " ", "twisted"]
    show (PNtog n1)                       = join ["p", show n1, "tog"]
    show (PNtogTwisted n1)                = join ["p", show n1, "tog", " ", "twisted"]
    show Sl1_k2tog_psso                   = "sl1-k2tog-psso"
    show Sl2_k1_p2sso                     = "sl2-k1-p2sso"
    show Sl1Wb                            = "sl1 wb"
    show P2so_yo_k1                       = "p2so-yo-k1"
    show P3so_k1_yo_k1                    = "p3so-k1-yo-k1"
    show P3so_k1_yo_ssk                   = "p3so-k1-yo-ssk"
    show Sl1_k1_yo_k1_psso                = "sl1-k1-yo-k1-psso"
    show (SlN_kN_yo_psso n1 n2)           = join ["sl", show n1, "-", "k", show n2, "-", "yo", "-", "psso"]
    show (SlN_kN_psso n1 n2)              = join ["sl", show n1, "-", "k", show n2, "-", "psso"]
    show (SlN_pN_psso n1 n2)              = join ["sl", show n1, "-", "p", show n2, "-", "psso"]
    show Sl1_p3so_k2tog_yo_k1             = "sl1-p3so-k2tog-yo-k1"
    show (Yo_kN_pyo n1)                   = join ["yo", "-", "k", show n1, "-", "pyo"]
    show (Yo_pN_pyo n1)                   = join ["yo", "-", "p", show n1, "-", "pyo"]
    show (N_to_NGather n1 n2)             = join [show n1, "-", "to", "-", show n2, " ", "gather"]
    show (WrapNSts n1)                    = join ["wrap", " ", show n1, " ", "sts"]
    show (N'NLeftThreadThru n1 n2)        = join [show n1, "/", show n2, " ", "left", " ", "thread", " ", "thru"]
    show (N'NPurledLeftThreadThru n1 n2)  = join [show n1, "/", show n2, " ", "purled", " ", "left", " ", "thread", " ", "thru"]
    show (N'NRightThreadThru n1 n2)       = join [show n1, "/", show n2, " ", "right", " ", "thread", " ", "thru"]
    show (N'NPurledRightThreadThru n1 n2) = join [show n1, "/", show n2, " ", "purled", " ", "right", " ", "thread", " ", "thru"]
    show (N'NLC n1 n2)                    = join [show n1, "/", show n2, " ", "LC"]
    show (N'NLPC n1 n2)                   = join [show n1, "/", show n2, " ", "LPC"]
    show (N'NLT n1 n2)                    = join [show n1, "/", show n2, " ", "LT"]
    show (N'NLPT n1 n2)                   = join [show n1, "/", show n2, " ", "LPT"]
    show (N'NLSC n1 n2)                   = join [show n1, "/", show n2, " ", "LSC"]
    show One'1LSAC                        = "1/1 LSAC"
    show (N'N'NLC n1 n2 n3)               = join [show n1, "/", show n2, "/", show n3, " ", "LC"]
    show (N'N'NLPC n1 n2 n3)              = join [show n1, "/", show n2, "/", show n3, " ", "LPC"]
    show One'1'1LT                        = "1/1/1 LT"
    show One'1'1LPT                       = "1/1/1 LPT"
    show (N'N'NLCC n1 n2 n3)              = join [show n1, "/", show n2, "/", show n3, " ", "LCC"]
    show (N'NRC n1 n2)                    = join [show n1, "/", show n2, " ", "RC"]
    show (N'NRPC n1 n2)                   = join [show n1, "/", show n2, " ", "RPC"]
    show (N'NRT n1 n2)                    = join [show n1, "/", show n2, " ", "RT"]
    show (N'NRPT n1 n2)                   = join [show n1, "/", show n2, " ", "RPT"]
    show (N'NRSC n1 n2)                   = join [show n1, "/", show n2, " ", "RSC"]
    show One'1RSAC                        = "1/1 RSAC"
    show (N'N'NRC n1 n2 n3)               = join [show n1, "/", show n2, "/", show n3, " ", "RC"]
    show (N'N'NRPC n1 n2 n3)              = join [show n1, "/", show n2, "/", show n3, " ", "RPC"]
    show One'1'1RT                        = "1/1/1 RT"
    show One'1'1RPT                       = "1/1/1 RPT"
    show (N'N'NRCC n1 n2 n3)              = join [show n1, "/", show n2, "/", show n3, " ", "RCC"]
    show K1Below                          = "k1 below"
    show P1Below                          = "p1 below"