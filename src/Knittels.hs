{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Knittels (Knittel (..), KName (..), KArity (..), TBL (..), InstructionNum, YarnPlacement (..)) where
import Control.Monad (join)

data Knittel = KInst KName InstructionNum KArity (Maybe TBL) 
    deriving (Eq, Read, Ord)

instance Show Knittel where
  show (KInst k n (KArity _) t) 
    | k ==  K || k == P = join [show k, show n, showTBL t]
    | otherwise = join [show k, showINum n, showTBL t]

showINum :: Int -> String
showINum 1 = ""
showINum n = " " ++ show n

showTBL :: Maybe TBL -> String
showTBL (Just TBL) = " tbl"
showTBL Nothing = ""

newtype KArity = KArity Int
  deriving (Show, Eq, Read, Ord)

data TBL = TBL
  deriving (Show, Eq, Read, Ord)

type InstructionNum = Int

data YarnPlacement = Wyif | Wyib
  deriving (Eq, Read, Ord)

instance Show YarnPlacement where
  show Wyif = " wyif"
  show Wyib = " wyib"

data KName
  = Knit
  | Purl
  | Slip InstructionNum YarnPlacement
  | K
  | P
  | Yo
  | Turn
    -- Generated from `generate_parser.py`
  | WAndt
  | Sssp --
  | Ssp --
  | Ssk --
  | Pfb 
  | Kfb
  | IncRp --
  | IncR --
  | IncLp --
  | IncL  --
  | DropSt 
  | DipSt
  | CtrDblInc
  | CddpTwisted 
  | Cddp 
  | CddTwisted 
  | BunnyEarsYo
  | BunnyEarsDec
  | BunnyEarsBackYo
  | BunnyEarsBackDec 
  | BrSl
  | SB
  | PBsl
  | PBp
  | PBk
  | PB
  | MK
  | MB
  | KBR --
  | KBL --
  | CO
  | BO --30

  | Yo_pN_pyo InstructionNum --
  | Yo_kN_pyo InstructionNum --
  | WrapNSts InstructionNum
  | Sl1_p3so_k2tog_yo_k1 -- Mirror of: P3so_k1_yo_ssk
  | SlN_pN_psso InstructionNum InstructionNum
  | Sl1_k2tog_psso
  | SlN_kN_yo_psso InstructionNum InstructionNum
  | Sl1_k1_yo_k1_psso
  | SlN_kN_psso InstructionNum InstructionNum
  | Sl2_k1_p2sso
  | Sl1Wb
  | PNtogTwisted InstructionNum --
  | PNtog InstructionNum --
  | P2so_yo_k1
  | P3so_k1_yo_ssk -- mirror of: Sl1_p3so_k2tog_yo_k1
  | P3so_k1_yo_k1
  | P1Below
  | N_to_NInc InstructionNum InstructionNum
  | N_to_NGather InstructionNum InstructionNum
  | N'NRightThreadThru InstructionNum InstructionNum
  | N'NRT InstructionNum InstructionNum
  | N'NRSC InstructionNum InstructionNum
  | One'1RSAC
  | N'NRPT InstructionNum InstructionNum
  | N'NRPC InstructionNum InstructionNum
  | N'NRC InstructionNum InstructionNum
  | N'NPurledRightThreadThru InstructionNum InstructionNum
  | N'NPurledLeftThreadThru InstructionNum InstructionNum
  | N'NLeftThreadThru InstructionNum InstructionNum
  | N'NLT InstructionNum InstructionNum
  | N'NLSC InstructionNum InstructionNum
  | One'1LSAC
  | N'NLPT InstructionNum InstructionNum
  | N'NLPC InstructionNum InstructionNum
  | N'NLC InstructionNum InstructionNum
  | One'1'1RT
  | One'1'1RPT
  | N'N'NRPC InstructionNum InstructionNum InstructionNum
  | N'N'NRCC InstructionNum InstructionNum InstructionNum
  | N'N'NRC InstructionNum InstructionNum InstructionNum
  | One'1'1LT
  | One'1'1LPT
  | N'N'NLPC InstructionNum InstructionNum InstructionNum
  | N'N'NLCC InstructionNum InstructionNum InstructionNum
  | N'N'NLC InstructionNum InstructionNum InstructionNum
  | M1Rp
  | M1R
  | M1Lp
  | M1L
  | KNtogTwisted InstructionNum
  | KNtog InstructionNum
  | K1Below
    deriving (Eq, Read, Ord)


instance Show KName where
  show (Slip n yp)                      = join ["sl", show n, show yp]
  show Knit                             = "Knit"
  show Purl                             = "Purl"
  show K                                = "k"
  show P                                = "p"
  show Yo                               = "yo"
  show Turn                             = "turn"

  -- Generated from `generate_parser.py
  show WAndt                            = "w&t"
  show Sssp                             = "sssp"
  show Ssp                              = "ssp"
  show Ssk                              = "ssk"
  show Pfb                              = "pfb"
  show Kfb                              = "kfb"
  show IncRp                            = "incRp"
  show IncR                             = "incR"
  show IncLp                            = "incLp"
  show IncL                             = "incL"
  show DropSt                           = "drop st"
  show DipSt                            = "dip st"
  show CtrDblInc                        = "ctr dbl inc"
  show CddpTwisted                      = "cddp twisted"
  show Cddp                             = "cddp"
  show CddTwisted                       = "cdd twisted"
  show BunnyEarsYo                      = "bunny ears yo"
  show BunnyEarsDec                     = "bunny ears dec"
  show BunnyEarsBackYo                  = "bunny ears back yo"
  show BunnyEarsBackDec                 = "bunny ears back dec"
  show BrSl                             = "brSl"
  show SB                               = "SB"
  show PBsl                             = "PBsl"
  show PBp                              = "PBp"
  show PBk                              = "PBk"
  show PB                               = "PB"
  show MK                               = "MK"
  show MB                               = "MB"
  show KBR                              = "KBR"
  show KBL                              = "KBL"
  show CO                               = "CO"
  show BO                               = "BO"

  show (Yo_pN_pyo n1)                   = join ["yo", "-", "p", show n1, "-", "pyo"]
  show (Yo_kN_pyo n1)                   = join ["yo", "-", "k", show n1, "-", "pyo"]
  show (WrapNSts n1)                    = join ["wrap", " ", show n1, " ", "sts"]
  show Sl1_p3so_k2tog_yo_k1             = "sl1-p3so-k2tog-yo-k1"
  show (SlN_pN_psso n1 n2)              = join ["sl", show n1, "-", "p", show n2, "-", "psso"]
  show Sl1_k2tog_psso                   = "sl1-k2tog-psso"
  show (SlN_kN_yo_psso n1 n2)           = join ["sl", show n1, "-", "k", show n2, "-", "yo", "-", "psso"]
  show Sl1_k1_yo_k1_psso                = "sl1-k1-yo-k1-psso"
  show (SlN_kN_psso n1 n2)              = join ["sl", show n1, "-", "k", show n2, "-", "psso"]
  show Sl2_k1_p2sso                     = "sl2-k1-p2sso"
  show Sl1Wb                            = "sl1 wb"
  show (PNtogTwisted n1)                = join ["p", show n1, "tog", " ", "twisted"]
  show (PNtog n1)                       = join ["p", show n1, "tog"]
  show P2so_yo_k1                       = "p2so-yo-k1"
  show P3so_k1_yo_ssk                   = "p3so-k1-yo-ssk"
  show P3so_k1_yo_k1                    = "p3so-k1-yo-k1"
  show P1Below                          = "p1 below"
  show (N_to_NInc n1 n2)                = join [show n1, "-", "to", "-", show n2, " ", "inc"]
  show (N_to_NGather n1 n2)             = join [show n1, "-", "to", "-", show n2, " ", "gather"]
  show (N'NRightThreadThru n1 n2)       = join [show n1, "/", show n2, " ", "right", " ", "thread", " ", "thru"]
  show (N'NRT n1 n2)                    = join [show n1, "/", show n2, " ", "RT"]
  show (N'NRSC n1 n2)                   = join [show n1, "/", show n2, " ", "RSC"]
  show One'1RSAC                        = "1/1 RSAC"
  show (N'NRPT n1 n2)                   = join [show n1, "/", show n2, " ", "RPT"]
  show (N'NRPC n1 n2)                   = join [show n1, "/", show n2, " ", "RPC"]
  show (N'NRC n1 n2)                    = join [show n1, "/", show n2, " ", "RC"]
  show (N'NPurledRightThreadThru n1 n2) = join [show n1, "/", show n2, " ", "purled", " ", "right", " ", "thread", " ", "thru"]
  show (N'NPurledLeftThreadThru n1 n2)  = join [show n1, "/", show n2, " ", "purled", " ", "left", " ", "thread", " ", "thru"]
  show (N'NLeftThreadThru n1 n2)        = join [show n1, "/", show n2, " ", "left", " ", "thread", " ", "thru"]
  show (N'NLT n1 n2)                    = join [show n1, "/", show n2, " ", "LT"]
  show (N'NLSC n1 n2)                   = join [show n1, "/", show n2, " ", "LSC"]
  show One'1LSAC                        = "1/1 LSAC"
  show (N'NLPT n1 n2)                   = join [show n1, "/", show n2, " ", "LPT"]
  show (N'NLPC n1 n2)                   = join [show n1, "/", show n2, " ", "LPC"]
  show (N'NLC n1 n2)                    = join [show n1, "/", show n2, " ", "LC"]
  show One'1'1RT                        = "1/1/1 RT"
  show One'1'1RPT                       = "1/1/1 RPT"
  show (N'N'NRPC n1 n2 n3)              = join [show n1, "/", show n2, "/", show n3, " ", "RPC"]
  show (N'N'NRCC n1 n2 n3)              = join [show n1, "/", show n2, "/", show n3, " ", "RCC"]
  show (N'N'NRC n1 n2 n3)               = join [show n1, "/", show n2, "/", show n3, " ", "RC"]
  show One'1'1LT                        = "1/1/1 LT"
  show One'1'1LPT                       = "1/1/1 LPT"
  show (N'N'NLPC n1 n2 n3)              = join [show n1, "/", show n2, "/", show n3, " ", "LPC"]
  show (N'N'NLCC n1 n2 n3)              = join [show n1, "/", show n2, "/", show n3, " ", "LCC"]
  show (N'N'NLC n1 n2 n3)               = join [show n1, "/", show n2, "/", show n3, " ", "LC"]
  show M1Rp                             = "M1Rp"
  show M1R                              = "M1R"
  show M1Lp                             = "M1Lp"
  show M1L                              = "M1L"
  show (KNtogTwisted n1)                = join ["k", show n1, "tog", " ", "twisted"]
  show (KNtog n1)                       = join ["k", show n1, "tog"]
  show K1Below                          = "k1 below"