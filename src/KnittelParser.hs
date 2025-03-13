module KnittelParser where
import KSSyntax ( Side(..) )
import Knittels
    ( KName(..),
      YarnPlacement(..),
      TBL(..),
      KArity(KArity),
      Knittel(..) )
import Text.ParserCombinators.Parsec
    ( Parser, notFollowedBy, (<|>), try )
import General ( num, parens, skipSymbol, symbol ) 

tbl :: Parser (Maybe TBL)
tbl =   try (do skipSymbol  "tbl"
                return (Just TBL))
        <|>
        return Nothing


knittel :: Parser Knittel
knittel =
    try(
    do  skipSymbol "k" <|> skipSymbol "K"
        n <- num
        notFollowedBy (symbol "tog")
        KInst (K n) (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "k" <|> skipSymbol "K"
        n1 <- num
        skipSymbol "tog"
        KInst (KNtog  n1) (KArity 2) <$> tbl)
    <|>
    try(
    do  skipSymbol "p" <|> skipSymbol "P"
        n <- num
        notFollowedBy (symbol "tog")
        KInst (P n) (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "Knit"
        KInst Knit (KArity (-1)) <$> tbl)
    <|>
    try(
    do  skipSymbol "Purl"
        KInst Purl (KArity (-1)) <$> tbl)
    <|>
    try(
    do  try (skipSymbol "Slip") <|> skipSymbol "sl"
        n <- num
        yp <- yarnPlacement 
        KInst (Slip n yp) (KArity 1) <$> tbl)
    <|>
    try(  
    do  skipSymbol "BO" 
        n <- num
        skipSymbol "sts"
        KInst (BO (Just n)) (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "BO"
        KInst (BO Nothing) (KArity 1) <$> tbl)
    <|>
    try(  -- TODO: make number optional 
    do  skipSymbol "CO" 
        n <- num
        skipSymbol "sts"
        KInst (CO (Just n)) (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "CO"
        KInst (CO Nothing) (KArity 1) <$> tbl)
    <|>
-- generated from `generate_parser.py`

    try(
    do  skipSymbol "ctr"
        skipSymbol "dbl"
        skipSymbol "inc"
        KInst CtrDblInc (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "incL"
        KInst IncL (KArity 0) <$> tbl)
    <|>
    try(
    do  skipSymbol "incLp"
        KInst IncLp (KArity 0) <$> tbl)
    <|>
    try(
    do  skipSymbol "incR"
        KInst IncR (KArity 0) <$> tbl)
    <|>
    try(
    do  skipSymbol "incRp"
        KInst IncRp (KArity 0) <$> tbl)
    <|>
    try(
    do  skipSymbol "kfb"
        KInst Kfb (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "pfb"
        KInst Pfb (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "yo"
        KInst Yo (KArity 0) <$> tbl)
    <|>
    try(
    do  skipSymbol "bunny"
        skipSymbol "ears"
        skipSymbol "back"
        skipSymbol "dec"
        KInst BunnyEarsBackDec (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "bunny"
        skipSymbol "ears"
        skipSymbol "back"
        skipSymbol "yo"
        KInst BunnyEarsBackYo (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "bunny"
        skipSymbol "ears"
        skipSymbol "dec"
        KInst BunnyEarsDec (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "bunny"
        skipSymbol "ears"
        skipSymbol "yo"
        KInst BunnyEarsYo (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "cdd"
        skipSymbol "twisted"
        KInst CddTwisted (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "cddp"
        KInst Cddp (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "cddp"
        skipSymbol "twisted"
        KInst CddpTwisted (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "ssk"
        KInst Ssk (KArity 2) <$> tbl)
    <|>
    try(
    do  skipSymbol "ssp"
        KInst Ssp (KArity 2) <$> tbl)
    <|>
    try(
    do  skipSymbol "sssp"
        KInst Sssp (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "KBL"
        KInst KBL (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "KBR"
        KInst KBR (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "PB"
        KInst PB (KArity 0) <$> tbl)
    <|>
    try(
    do  skipSymbol "PBk"
        KInst PBk (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "PBp"
        KInst PBp (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "PBsl"
        KInst PBsl (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "SB"
        KInst SB (KArity 0) <$> tbl)
    <|>
    try(
    do  skipSymbol "brSl"
        KInst BrSl (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "dip"
        skipSymbol "st"
        KInst DipSt (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "drop"
        skipSymbol "st"
        KInst DropSt (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "MB"
        KInst MB (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "MK"
        KInst MK (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "w&t"
        KInst WAndt (KArity 1) <$> tbl)
    <|>

    -- Combined knittels: 1_to_4Inc 1_to_5Inc 1_to_6Inc 1_to_7Inc 1_to_8Inc 1_to_9Inc
    try (
    do  n1 <- num
        skipSymbol "-"
        skipSymbol "to"
        skipSymbol "-"
        n2 <- num
        skipSymbol "inc"
        KInst (N_to_NInc  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try(
    do  skipSymbol "M1L"
        KInst M1L (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "M1Lp"
        KInst M1Lp (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "M1R"
        KInst M1R (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "M1Rp"
        KInst M1Rp (KArity 1) <$> tbl)
    <|>
    -- Combined knittels: K2togTwisted K3togTwisted
    try (
    do  skipSymbol "k"
        n1 <- num
        skipSymbol "tog"
        skipSymbol "twisted"
        KInst (KNtogTwisted  n1) (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined knittels: P2tog P3tog P4tog P5tog P6tog P7tog P8tog P9tog
    try (
    do  skipSymbol "p"
        n1 <- num
        skipSymbol "tog"
        KInst (PNtog  n1) (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined knittels: P2togTwisted P3togTwisted
    try (
    do  skipSymbol "p"
        n1 <- num
        skipSymbol "tog"
        skipSymbol "twisted"
        KInst (PNtogTwisted  n1) (KArity (sum [n1])) <$> tbl)
    <|>
    try(
    do  skipSymbol "sl1-k2tog-psso"
        KInst Sl1_k2tog_psso (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "sl2-k1-p2sso"
        KInst Sl2_k1_p2sso (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "sl1"
        skipSymbol "wb"
        KInst Sl1Wb (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "p2so-yo-k1"
        KInst P2so_yo_k1 (KArity 2) <$> tbl)
    <|>
    try(
    do  skipSymbol "p3so-k1-yo-k1"
        KInst P3so_k1_yo_k1 (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "p3so-k1-yo-ssk"
        KInst P3so_k1_yo_ssk (KArity 4) <$> tbl)
    <|>
    try(
    do  skipSymbol "sl1-k1-yo-k1-psso"
        KInst Sl1_k1_yo_k1_psso (KArity 3) <$> tbl)
    <|>
    -- Combined knittels: Sl1_k1_yo_psso Sl1_k2_yo_psso
    try (
    do  skipSymbol "sl"
        n1 <- num
        skipSymbol "-"
        skipSymbol "k"
        n2 <- num
        skipSymbol "-"
        skipSymbol "yo"
        skipSymbol "-"
        skipSymbol "psso"
        KInst (SlN_kN_yo_psso  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: Sl1_k2_psso Sl1_k3_psso
    try (
    do  skipSymbol "sl"
        n1 <- num
        skipSymbol "-"
        skipSymbol "k"
        n2 <- num
        skipSymbol "-"
        skipSymbol "psso"
        KInst (SlN_kN_psso  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: Sl1_p2_psso Sl1_p3_psso
    try (
    do  skipSymbol "sl"
        n1 <- num
        skipSymbol "-"
        skipSymbol "p"
        n2 <- num
        skipSymbol "-"
        skipSymbol "psso"
        KInst (SlN_pN_psso  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try(
    do  skipSymbol "sl1-p3so-k2tog-yo-k1"
        KInst Sl1_p3so_k2tog_yo_k1 (KArity 4) <$> tbl)
    <|>
    -- Combined knittels: Yo_k2_pyo Yo_k3_pyo
    try (
    do  skipSymbol "yo"
        skipSymbol "-"
        skipSymbol "k"
        n1 <- num
        skipSymbol "-"
        skipSymbol "pyo"
        KInst (Yo_kN_pyo  n1) (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined knittels: Yo_p2_pyo Yo_p3_pyo
    try (
    do  skipSymbol "yo"
        skipSymbol "-"
        skipSymbol "p"
        n1 <- num
        skipSymbol "-"
        skipSymbol "pyo"
        KInst (Yo_pN_pyo  n1) (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined knittels: 2_to_2Gather 2_to_3Gather 2_to_5Gather 2_to_7Gather 2_to_9Gather 3_to_2Gather 3_to_3Gather 3_to_5Gather 3_to_7Gather 3_to_9Gather 5_to_3Gather 5_to_5Gather 5_to_7Gather 5_to_9Gather 7_to_3Gather 7_to_5Gather 7_to_7Gather 7_to_9Gather
    try (
    do  n1 <- num
        skipSymbol "-"
        skipSymbol "to"
        skipSymbol "-"
        n2 <- num
        skipSymbol "gather"
        KInst (N_to_NGather  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: Wrap2Sts Wrap3Sts Wrap4Sts Wrap5Sts Wrap6Sts Wrap7Sts Wrap8Sts Wrap9Sts
    try (
    do  skipSymbol "wrap"
        n1 <- num
        skipSymbol "sts"
        KInst (WrapNSts  n1) (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined knittels: 1'1LeftThreadThru 2'2LeftThreadThru 3'3LeftThreadThru 4'4LeftThreadThru 5'5LeftThreadThru
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "left"
        skipSymbol "thread"
        skipSymbol "thru"
        KInst (N'NLeftThreadThru  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1PurledLeftThreadThru 2'2PurledLeftThreadThru 3'3PurledLeftThreadThru 4'4PurledLeftThreadThru 5'5PurledLeftThreadThru
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "purled"
        skipSymbol "left"
        skipSymbol "thread"
        skipSymbol "thru"
        KInst (N'NPurledLeftThreadThru  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1RightThreadThru 2'2RightThreadThru 3'3RightThreadThru 4'4RightThreadThru 5'5RightThreadThru
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "right"
        skipSymbol "thread"
        skipSymbol "thru"
        KInst (N'NRightThreadThru  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1PurledRightThreadThru 2'2PurledRightThreadThru 3'3PurledRightThreadThru 4'4PurledRightThreadThru 5'5PurledRightThreadThru
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "purled"
        skipSymbol "right"
        skipSymbol "thread"
        skipSymbol "thru"
        KInst (N'NPurledRightThreadThru  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1LC 1'2LC 1'3LC 2'1LC 2'2LC 2'3LC 3'1LC 3'2LC 3'3LC 4'1LC 4'2LC 4'3LC 4'4LC 5'5LC 6'6LC 7'7LC 8'8LC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LC"
        KInst (N'NLC  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1LPC 1'2LPC 1'3LPC 2'1LPC 2'2LPC 2'3LPC 3'1LPC 3'2LPC 3'3LPC 4'1LPC 4'2LPC 4'3LPC 4'4LPC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LPC"
        KInst (N'NLPC  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1LT 1'2LT 2'1LT
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LT"
        KInst (N'NLT  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1LPT 1'2LPT 2'1LPT
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LPT"
        KInst (N'NLPT  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1LSC 1'2LSC 1'3LSC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LSC"
        KInst (N'NLSC  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try(
    do  skipSymbol "1/1"
        skipSymbol "LSAC"
        KInst One'1LSAC (KArity 2) <$> tbl)
    <|>
    -- Combined knittels: 1'1'1LC 1'2'1LC 2'1'2LC 2'2'2LC 3'1'3LC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "LC"
        KInst (N'N'NLC  n1 n2 n3) (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    -- Combined knittels: 1'1'1LPC 1'2'1LPC 2'1'2LPC 2'2'2LPC 3'1'3LPC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "LPC"
        KInst (N'N'NLPC  n1 n2 n3) (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    try(
    do  skipSymbol "1/1/1"
        skipSymbol "LT"
        KInst One'1'1LT (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "1/1/1"
        skipSymbol "LPT"
        KInst One'1'1LPT (KArity 3) <$> tbl)
    <|>
    -- Combined knittels: 1'1'1LCC 1'2'1LCC 2'1'2LCC 2'2'2LCC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "LCC"
        KInst (N'N'NLCC  n1 n2 n3) (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    -- Combined knittels: 1'1RC 1'2RC 1'3RC 2'1RC 2'2RC 2'3RC 3'1RC 3'2RC 3'3RC 4'1RC 4'2RC 4'3RC 4'4RC 5'5RC 6'6RC 7'7RC 8'8RC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RC"
        KInst (N'NRC  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1RPC 1'2RPC 1'3RPC 2'1RPC 2'2RPC 2'3RPC 3'1RPC 3'2RPC 3'3RPC 4'1RPC 4'2RPC 4'3RPC 4'4RPC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RPC"
        KInst (N'NRPC  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1RT 1'2RT 2'1RT
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RT"
        KInst (N'NRT  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1RPT 1'2RPT 2'1RPT
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RPT"
        KInst (N'NRPT  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined knittels: 1'1RSC 1'2RSC 1'3RSC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RSC"
        KInst (N'NRSC  n1 n2) (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try(
    do  skipSymbol "1/1"
        skipSymbol "RSAC"
        KInst One'1RSAC (KArity 2) <$> tbl)
    <|>
    -- Combined knittels: 1'1'1RC 1'2'1RC 2'1'2RC 2'2'2RC 3'1'3RC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "RC"
        KInst (N'N'NRC  n1 n2 n3) (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    -- Combined knittels: 1'1'1RPC 1'2'1RPC 2'1'2RPC 2'2'2RPC 3'1'3RPC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "RPC"
        KInst (N'N'NRPC  n1 n2 n3) (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    try(
    do  skipSymbol "1/1/1"
        skipSymbol "RT"
        KInst One'1'1RT (KArity 3) <$> tbl)
    <|>
    try(
    do  skipSymbol "1/1/1"
        skipSymbol "RPT"
        KInst One'1'1RPT (KArity 3) <$> tbl)
    <|>
    -- Combined knittels: 1'1'1RCC 1'2'1RCC 2'1'2RCC 2'2'2RCC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "RCC"
        KInst (N'N'NRCC  n1 n2 n3) (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    try(
    do  skipSymbol "k1"
        skipSymbol "below"
        KInst K1Below (KArity 0) <$> tbl)
    <|>
    try(
    do  skipSymbol "p1"
        skipSymbol "below"
        KInst P1Below (KArity 0) <$> tbl)

yarnPlacement :: Parser YarnPlacement
yarnPlacement =
    try(
    do  skipSymbol "wyif"
        return Wyif)
    <|>
    do  skipSymbol "wyib"
        return Wyib


side :: Parser Side
side =
    try(
    do  parens (skipSymbol "RS")
        return R)
    <|>
    do  parens (skipSymbol "WS")
        return W
    <|>
    do  return None
