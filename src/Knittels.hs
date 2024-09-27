-- TODO: utvid 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- må sjekke om de er negative 
-- er det like greit å bare ha Integers også sjekke at de ikke er 0? har vi lov å ha 0? Skal vi bare skippe da eller si feil? 
module Knittels where
type InstructionNum = Integer 

data YarnPlacement = Wyif | Wyib
    deriving (Eq, Show, Read)


data Knittel = -- riktig bruk av ordet knittel? Blir det slitsomt å definer alle 252 på denne måten? 
          K     InstructionNum
        | P     InstructionNum
        | Slip  InstructionNum YarnPlacement
        | Knit  -- NOTE: skal kanskje være en instruction fordi den omhandler hele raden
        | Purl  -- NOTE: samme som over
        | Tbl
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
        | BO
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
        | KNtog InstructionNum
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