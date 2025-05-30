module KnittelParser (tbl, knittel, yarnPlacement) where

import General (maybeINum, num, skipSymbol)
import Knittels
  ( KArity (KArity),
    KName (..),
    Knittel (..),
    TBL (..),
    YarnPlacement (..),
  )
import Text.Parsec.String (Parser)
import Text.Parsec
  ( notFollowedBy,
    try,
    (<|>), 
    string)

tbl :: Parser (Maybe TBL)
tbl =
  try
    ( do
        skipSymbol "tbl"
        return (Just TBL)
    )
    <|> return Nothing

knittel :: Parser Knittel
knittel =
    try(
    do  skipSymbol "k"
        notFollowedBy (string "f" <|> string "B")
        n <- maybeINum
        notFollowedBy (string "tog" <|> string "s" <|> string "below")
        KInst K n (KArity 1) <$> tbl)
    <|> 
    try(
    do  skipSymbol "p"
        notFollowedBy (string "f" <|> string "B")
        n <- maybeINum
        notFollowedBy (string "tog" <|> string "s" <|> string "below")
        KInst P n (KArity 1) <$> tbl)
    <|>
    try(
    do  skipSymbol "Knit" 
        KInst Knit 0 (KArity (-1)) <$> tbl)
    <|> 
    try(
    do  skipSymbol "Purl"
        KInst Purl 0 (KArity (-1)) <$> tbl)
    <|> 
    try(
    do  try (skipSymbol "Slip") <|> skipSymbol "sl"
        n <- num
        yp <- yarnPlacement
        KInst (Slip n yp) n (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "yo"
        notFollowedBy (string "-")
        r <- maybeINum
        KInst Yo r (KArity 0) <$> tbl)
    <|>
    try (
    do  skipSymbol "turn"
        KInst Turn 1 (KArity 0) <$> tbl)
    
    -- Generated from `generate_parser.py
    <|>
    try (
    do  skipSymbol "w&t"
        r <- maybeINum
        KInst WAndt r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "sssp"
        r <- maybeINum
        KInst Sssp r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "ssp"
        r <- maybeINum
        KInst Ssp r (KArity 2) <$> tbl)
    <|>
    try (
    do  skipSymbol "ssk"
        r <- maybeINum
        KInst Ssk r (KArity 2) <$> tbl)
    <|>
    try (
    do  skipSymbol "pfb"
        r <- maybeINum
        KInst Pfb r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "kfb"
        r <- maybeINum
        KInst Kfb r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "incRp"
        r <- maybeINum
        KInst IncRp r (KArity 0) <$> tbl)
    <|>
    try (
    do  skipSymbol "incR"
        r <- maybeINum
        KInst IncR r (KArity 0) <$> tbl)
    <|>
    try (
    do  skipSymbol "incLp"
        r <- maybeINum
        KInst IncLp r (KArity 0) <$> tbl)
    <|>
    try (
    do  skipSymbol "incL"
        r <- maybeINum
        KInst IncL r (KArity 0) <$> tbl)
    <|>
    try (
    do  skipSymbol "drop"
        skipSymbol "st"
        r <- maybeINum
        KInst DropSt r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "dip"
        skipSymbol "st"
        r <- maybeINum
        KInst DipSt r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "ctr"
        skipSymbol "dbl"
        skipSymbol "inc"
        r <- maybeINum
        KInst CtrDblInc r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "cddp"
        skipSymbol "twisted"
        r <- maybeINum
        KInst CddpTwisted r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "cddp"
        r <- maybeINum
        KInst Cddp r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "cdd"
        skipSymbol "twisted"
        r <- maybeINum
        KInst CddTwisted r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "bunny"
        skipSymbol "ears"
        skipSymbol "yo"
        r <- maybeINum
        KInst BunnyEarsYo r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "bunny"
        skipSymbol "ears"
        skipSymbol "dec"
        r <- maybeINum
        KInst BunnyEarsDec r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "bunny"
        skipSymbol "ears"
        skipSymbol "back"
        skipSymbol "yo"
        r <- maybeINum
        KInst BunnyEarsBackYo r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "bunny"
        skipSymbol "ears"
        skipSymbol "back"
        skipSymbol "dec"
        r <- maybeINum
        KInst BunnyEarsBackDec r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "brSl"
        r <- maybeINum
        KInst BrSl r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "SB"
        r <- maybeINum
        KInst SB r (KArity 0) <$> tbl)
    <|>
    try (
    do  skipSymbol "PBsl"
        r <- maybeINum
        KInst PBsl r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "PBp"
        r <- maybeINum
        KInst PBp r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "PBk"
        r <- maybeINum
        KInst PBk r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "PB"
        r <- maybeINum
        KInst PB r (KArity 0) <$> tbl)
    <|>
    try (
    do  skipSymbol "MK"
        r <- maybeINum
        KInst MK r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "MB"
        r <- maybeINum
        KInst MB r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "KBR"
        r <- maybeINum
        KInst KBR r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "KBL"
        r <- maybeINum
        KInst KBL r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "CO"
        r <- maybeINum
        KInst CO r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "BO"
        r <- maybeINum
        KInst BO r (KArity 1) <$> tbl)
    <|>
    
    -- Combined operations: yo-p2-pyo, yo-p3-pyo
    try (
    do  skipSymbol "yo"
        skipSymbol "-"
        skipSymbol "p"
        n1 <- num
        skipSymbol "-"
        skipSymbol "pyo"
        r <- maybeINum
        KInst (Yo_pN_pyo n1) r (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined operations: yo-k2-pyo, yo-k3-pyo
    try (
    do  skipSymbol "yo"
        skipSymbol "-"
        skipSymbol "k"
        n1 <- num
        skipSymbol "-"
        skipSymbol "pyo"
        r <- maybeINum
        KInst (Yo_kN_pyo n1) r (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined operations: wrap 2 sts, wrap 3 sts, wrap 4 sts, wrap 5 sts, wrap 6 sts, wrap 7 sts, wrap 8 sts, wrap 9 sts
    try (
    do  skipSymbol "wrap"
        n1 <- num
        skipSymbol "sts"
        r <- maybeINum
        KInst (WrapNSts n1) r (KArity (sum [n1])) <$> tbl)
    <|>
    try (
    do  skipSymbol "sl1-p3so-k2tog-yo-k1"
        r <- maybeINum
        KInst Sl1_p3so_k2tog_yo_k1 r (KArity 4) <$> tbl)
    <|>
    -- Combined operations: sl1-p2-psso, sl1-p3-psso
    try (
    do  skipSymbol "sl"
        n1 <- num
        skipSymbol "-"
        skipSymbol "p"
        n2 <- num
        skipSymbol "-"
        skipSymbol "psso"
        r <- maybeINum
        KInst (SlN_pN_psso n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try (
    do  skipSymbol "sl1-k2tog-psso"
        r <- maybeINum
        KInst Sl1_k2tog_psso r (KArity 3) <$> tbl)
    <|>
    -- Combined operations: sl1-k1-yo-psso, sl1-k2-yo-psso
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
        r <- maybeINum
        KInst (SlN_kN_yo_psso n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try (
    do  skipSymbol "sl1-k1-yo-k1-psso"
        r <- maybeINum
        KInst Sl1_k1_yo_k1_psso r (KArity 3) <$> tbl)
    <|>
    -- Combined operations: sl1-k2-psso, sl1-k3-psso
    try (
    do  skipSymbol "sl"
        n1 <- num
        skipSymbol "-"
        skipSymbol "k"
        n2 <- num
        skipSymbol "-"
        skipSymbol "psso"
        r <- maybeINum
        KInst (SlN_kN_psso n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try (
    do  skipSymbol "sl2-k1-p2sso"
        r <- maybeINum
        KInst Sl2_k1_p2sso r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "sl1"
        skipSymbol "wb"
        r <- maybeINum
        KInst Sl1Wb r (KArity 1) <$> tbl)
    <|>
    -- Combined operations: p2tog twisted, p3tog twisted
    try (
    do  skipSymbol "p"
        n1 <- num
        skipSymbol "tog"
        skipSymbol "twisted"
        r <- maybeINum
        KInst (PNtogTwisted n1) r (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined operations: p2tog, p3tog, p4tog, p5tog, p6tog, p7tog, p8tog, p9tog
    try (
    do  skipSymbol "p"
        n1 <- num
        skipSymbol "tog"
        r <- maybeINum
        KInst (PNtog n1) r (KArity (sum [n1])) <$> tbl)
    <|>
    try (
    do  skipSymbol "p2so-yo-k1"
        r <- maybeINum
        KInst P2so_yo_k1 r (KArity 2) <$> tbl)
    <|>
    try (
    do  skipSymbol "p3so-k1-yo-ssk"
        r <- maybeINum
        KInst P3so_k1_yo_ssk r (KArity 4) <$> tbl)
    <|>
    try (
    do  skipSymbol "p3so-k1-yo-k1"
        r <- maybeINum
        KInst P3so_k1_yo_k1 r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "p1"
        skipSymbol "below"
        r <- maybeINum
        KInst P1Below r (KArity 1) <$> tbl)
    <|>
    -- Combined operations: 1-to-4 inc, 1-to-5 inc, 1-to-6 inc, 1-to-7 inc, 1-to-8 inc, 1-to-9 inc
    try (
    do  n1 <- num
        skipSymbol "-"
        skipSymbol "to"
        skipSymbol "-"
        n2 <- num
        skipSymbol "inc"
        r <- maybeINum
        KInst (N_to_NInc n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 2-to-2 gather, 2-to-3 gather, 2-to-5 gather, 2-to-7 gather, 2-to-9 gather, 3-to-2 gather, 3-to-3 gather, 3-to-5 gather, 3-to-7 gather, 3-to-9 gather, 5-to-3 gather, 5-to-5 gather, 5-to-7 gather, 5-to-9 gather, 7-to-3 gather, 7-to-5 gather, 7-to-7 gather, 7-to-9 gather
    try (
    do  n1 <- num
        skipSymbol "-"
        skipSymbol "to"
        skipSymbol "-"
        n2 <- num
        skipSymbol "gather"
        r <- maybeINum
        KInst (N_to_NGather n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 right thread thru, 2/2 right thread thru, 3/3 right thread thru, 4/4 right thread thru, 5/5 right thread thru
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "right"
        skipSymbol "thread"
        skipSymbol "thru"
        r <- maybeINum
        KInst (N'NRightThreadThru n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 RT, 1/2 RT, 2/1 RT
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RT"
        r <- maybeINum
        KInst (N'NRT n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 RSC, 1/2 RSC, 1/3 RSC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RSC"
        r <- maybeINum
        KInst (N'NRSC n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try (
    do  skipSymbol "1/1"
        skipSymbol "RSAC"
        r <- maybeINum
        KInst One'1RSAC r (KArity 2) <$> tbl)
    <|>
    -- Combined operations: 1/1 RPT, 1/2 RPT, 2/1 RPT
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RPT"
        r <- maybeINum
        KInst (N'NRPT n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 RPC, 1/2 RPC, 1/3 RPC, 2/1 RPC, 2/2 RPC, 2/3 RPC, 3/1 RPC, 3/2 RPC, 3/3 RPC, 4/1 RPC, 4/2 RPC, 4/3 RPC, 4/4 RPC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RPC"
        r <- maybeINum
        KInst (N'NRPC n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 RC, 1/2 RC, 1/3 RC, 2/1 RC, 2/2 RC, 2/3 RC, 3/1 RC, 3/2 RC, 3/3 RC, 4/1 RC, 4/2 RC, 4/3 RC, 4/4 RC, 5/5 RC, 6/6 RC, 7/7 RC, 8/8 RC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "RC"
        r <- maybeINum
        KInst (N'NRC n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 purled right thread thru, 2/2 purled right thread thru, 3/3 purled right thread thru, 4/4 purled right thread thru, 5/5 purled right thread thru
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "purled"
        skipSymbol "right"
        skipSymbol "thread"
        skipSymbol "thru"
        r <- maybeINum
        KInst (N'NPurledRightThreadThru n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 purled left thread thru, 2/2 purled left thread thru, 3/3 purled left thread thru, 4/4 purled left thread thru, 5/5 purled left thread thru
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "purled"
        skipSymbol "left"
        skipSymbol "thread"
        skipSymbol "thru"
        r <- maybeINum
        KInst (N'NPurledLeftThreadThru n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 left thread thru, 2/2 left thread thru, 3/3 left thread thru, 4/4 left thread thru, 5/5 left thread thru
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "left"
        skipSymbol "thread"
        skipSymbol "thru"
        r <- maybeINum
        KInst (N'NLeftThreadThru n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 LT, 1/2 LT, 2/1 LT
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LT"
        r <- maybeINum
        KInst (N'NLT n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 LSC, 1/2 LSC, 1/3 LSC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LSC"
        r <- maybeINum
        KInst (N'NLSC n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try (
    do  skipSymbol "1/1"
        skipSymbol "LSAC"
        r <- maybeINum
        KInst One'1LSAC r (KArity 2) <$> tbl)
    <|>
    -- Combined operations: 1/1 LPT, 1/2 LPT, 2/1 LPT
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LPT"
        r <- maybeINum
        KInst (N'NLPT n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 LPC, 1/2 LPC, 1/3 LPC, 2/1 LPC, 2/2 LPC, 2/3 LPC, 3/1 LPC, 3/2 LPC, 3/3 LPC, 4/1 LPC, 4/2 LPC, 4/3 LPC, 4/4 LPC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LPC"
        r <- maybeINum
        KInst (N'NLPC n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    -- Combined operations: 1/1 LC, 1/2 LC, 1/3 LC, 2/1 LC, 2/2 LC, 2/3 LC, 3/1 LC, 3/2 LC, 3/3 LC, 4/1 LC, 4/2 LC, 4/3 LC, 4/4 LC, 5/5 LC, 6/6 LC, 7/7 LC, 8/8 LC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "LC"
        r <- maybeINum
        KInst (N'NLC n1 n2) r (KArity (sum [n1, n2])) <$> tbl)
    <|>
    try (
    do  skipSymbol "1/1/1"
        skipSymbol "RT"
        r <- maybeINum
        KInst One'1'1RT r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "1/1/1"
        skipSymbol "RPT"
        r <- maybeINum
        KInst One'1'1RPT r (KArity 3) <$> tbl)
    <|>
    -- Combined operations: 1/1/1 RPC, 1/2/1 RPC, 2/1/2 RPC, 2/2/2 RPC, 3/1/3 RPC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "RPC"
        r <- maybeINum
        KInst (N'N'NRPC n1 n2 n3) r (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    -- Combined operations: 1/1/1 RCC, 1/2/1 RCC, 2/1/2 RCC, 2/2/2 RCC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "RCC"
        r <- maybeINum
        KInst (N'N'NRCC n1 n2 n3) r (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    -- Combined operations: 1/1/1 RC, 1/2/1 RC, 2/1/2 RC, 2/2/2 RC, 3/1/3 RC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "RC"
        r <- maybeINum
        KInst (N'N'NRC n1 n2 n3) r (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    try (
    do  skipSymbol "1/1/1"
        skipSymbol "LT"
        r <- maybeINum
        KInst One'1'1LT r (KArity 3) <$> tbl)
    <|>
    try (
    do  skipSymbol "1/1/1"
        skipSymbol "LPT"
        r <- maybeINum
        KInst One'1'1LPT r (KArity 3) <$> tbl)
    <|>
    -- Combined operations: 1/1/1 LPC, 1/2/1 LPC, 2/1/2 LPC, 2/2/2 LPC, 3/1/3 LPC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "LPC"
        r <- maybeINum
        KInst (N'N'NLPC n1 n2 n3) r (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    -- Combined operations: 1/1/1 LCC, 1/2/1 LCC, 2/1/2 LCC, 2/2/2 LCC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "LCC"
        r <- maybeINum
        KInst (N'N'NLCC n1 n2 n3) r (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    -- Combined operations: 1/1/1 LC, 1/2/1 LC, 2/1/2 LC, 2/2/2 LC, 3/1/3 LC
    try (
    do  n1 <- num
        skipSymbol "/"
        n2 <- num
        skipSymbol "/"
        n3 <- num
        skipSymbol "LC"
        r <- maybeINum
        KInst (N'N'NLC n1 n2 n3) r (KArity (sum [n1, n2, n3])) <$> tbl)
    <|>
    try (
    do  skipSymbol "M1Rp"
        r <- maybeINum
        KInst M1Rp r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "M1R"
        r <- maybeINum
        KInst M1R r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "M1Lp"
        r <- maybeINum
        KInst M1Lp r (KArity 1) <$> tbl)
    <|>
    try (
    do  skipSymbol "M1L"
        r <- maybeINum
        KInst M1L r (KArity 1) <$> tbl)
    <|>
    -- Combined operations: k2tog twisted, k3tog twisted
    try (
    do  skipSymbol "k"
        n1 <- num
        skipSymbol "tog"
        skipSymbol "twisted"
        r <- maybeINum
        KInst (KNtogTwisted n1) r (KArity (sum [n1])) <$> tbl)
    <|>
    -- Combined operations: k2tog, k3tog, k4tog, k5tog, k6tog, k7tog, k8tog, k9tog
    try (
    do  skipSymbol "k"
        n1 <- num
        skipSymbol "tog"
        r <- maybeINum
        KInst (KNtog n1) r (KArity (sum [n1])) <$> tbl)
    <|>
    try (
    do  skipSymbol "k1"
        skipSymbol "below"
        r <- maybeINum
        KInst K1Below r (KArity 1) <$> tbl)


yarnPlacement :: Parser YarnPlacement
yarnPlacement =
    try(
    do  skipSymbol "wyif"
        return Wyif)
    <|>
    do  skipSymbol "wyib"
        return Wyib