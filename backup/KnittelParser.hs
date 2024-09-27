module KnittelParser where

knittel :: Parser Knittel
knittel =
    try (lexeme (
    do  _ <- string "k" <|> string "K"
        n <- num
        notFollowedBy (symbol "tog")
        return (K n)))
    <|>
    try (
    do  _ <- string "k" <|> string "K"
        n <- num
        skipSymbol "tog"
        return (KNtog n))
    <|>
    try (
    do  _ <- string "p" <|> string "P"
        n <- lexeme num
        return (P n))
    <|>
    do  skipSymbol "yo" <|> skipSymbol "Yo"
        return Yo
    <|>
    do  skipSymbol "kfb"
        return Kfb
    <|>
    do  skipSymbol "ssk"
        return Ssk
    <|>
    do  skipSymbol "Knit"
        return Knit
    <|>
    do  skipSymbol "Purl"
        return Purl
    <|>
    do  try (skipSymbol "Slip") <|> skipSymbol "sl"
        n <- num
        Slip n <$> yarnPlacement
	<|>
	do	try (skipSymbol "tbl")
		return Tbl
	<|>
	do	try (skipSymbol "CO")
		return CO
	<|>
	do	try (skipSymbol "ctr"
		  skipSymbol "dbl"
		  skipSymbol "inc")
		return CtrDblInc
	<|>
	do	try (skipSymbol "incL")
		return IncL
	<|>
	do	try (skipSymbol "incLp")
		return IncLp
	<|>
	do	try (skipSymbol "incR")
		return IncR
	<|>
	do	try (skipSymbol "incRp")
		return IncRp
	<|>
	do	try (skipSymbol "kfb")
		return Kfb
	<|>
	do	try (skipSymbol "pfb")
		return Pfb
	<|>
	do	try (skipSymbol "st")
		return St
	<|>
	do	try (skipSymbol "yo")
		return Yo
	<|>
	do	try (skipSymbol "BO")
		return BO
	<|>
	do	try (skipSymbol "bunny"
		skipSymbol "ears"
		skipSymbol "back"
		skipSymbol "dec")
		return BunnyEarsBackDec
	<|>
	do	try (skipSymbol "bunny"
		skipSymbol "ears"
		skipSymbol "back"
		skipSymbol "yo")
		return BunnyEarsBackYo
	<|>
	do	try (skipSymbol "bunny"
		skipSymbol "ears"
		skipSymbol "dec")
		return BunnyEarsDec
	<|>
	do	try (skipSymbol "bunny"
		skipSymbol "ears"
		skipSymbol "yo")
		return BunnyEarsYo
	<|>
	do	try (skipSymbol "cdd"
		skipSymbol "twisted")
		return CddTwisted
	<|>
	do	try (skipSymbol "cddp")
		return Cddp
	<|>
	do	try (skipSymbol "cddp"
		skipSymbol "twisted")
		return CddpTwisted
	<|>
	do	try (skipSymbol "ssk")
		return Ssk
	<|>
	do	try (skipSymbol "ssp")
		return Ssp
	<|>
	do	try (skipSymbol "sssp")
		return Sssp
	<|>
	do	try (skipSymbol "KBL")
		return KBL
	<|>
	do	try (skipSymbol "KBR")
		return KBR
	<|>
	do	try (skipSymbol "PB")
		return PB
	<|>
	do	try (skipSymbol "PBk")
		return PBk
	<|>
	do	try (skipSymbol "PBp")
		return PBp
	<|>
	do	try (skipSymbol "PBsl")
		return PBsl
	<|>
	do	try (skipSymbol "SB")
		return SB
	<|>
	do	try (skipSymbol "cn")
		return Cn
	<|>
	do	try (skipSymbol "sts")
		return Sts
	<|>
	do	try (skipSymbol "brSl")
		return BrSl
	<|>
	do	try (skipSymbol "dip"
		skipSymbol "st")
		return DipSt
	<|>
	do	try (skipSymbol "drop"
		skipSymbol "st")
		return DropSt
	<|>
	do	try (skipSymbol "MB")
		return MB
	<|>
	do	try (skipSymbol "MK")
		return MK
	<|>
	do	try (skipSymbol "w&t")
		return WAndt
	<|>

		-- Combined knittels: 1_to_4Inc 1_to_5Inc 1_to_6Inc 1_to_7Inc 1_to_8Inc 1_to_9Inc
	do	try (
		n1 <- num
		string "-"
		string "to"
		string "-"
		n2 <- num
		
		skipSymbol "inc")
		return N_to_NInc	n1 n2
	<|>
	do	try (skipSymbol "M1L")
		return M1L
	<|>
	do	try (skipSymbol "M1Lp")
		return M1Lp
	<|>
	do	try (skipSymbol "M1R")
		return M1R
	<|>
	do	try (skipSymbol "M1Rp")
		return M1Rp
	<|>
		-- Combined knittels: K2tog K3tog K4tog K5tog K6tog K7tog K8tog K9tog
	do	try (
		string "k"
		n1 <- num
		skipSymbol "tog")
		return KNtog	n1
	<|>
		-- Combined knittels: K2togTwisted K3togTwisted
	do	try (
		string "k"
		n1 <- num
		skipSymbol "tog"
		
		skipSymbol "twisted")
		return KNtogTwisted	n1
	<|>
		-- Combined knittels: P2tog P3tog P4tog P5tog P6tog P7tog P8tog P9tog
	do	try (
		string "p"
		n1 <- num
		skipSymbol "tog")
		return PNtog	n1
	<|>
		-- Combined knittels: P2togTwisted P3togTwisted
	do	try (
		string "p"
		n1 <- num
		skipSymbol "tog"
		
		skipSymbol "twisted")
		return PNtogTwisted	n1
	<|>
	do	try (skipSymbol "sl1-k2tog-psso")
		return Sl1_k2tog_psso
	<|>
	do	try (skipSymbol "sl2-k1-p2sso")
		return Sl2_k1_p2sso
	<|>
	do	try (skipSymbol "sl1"
		skipSymbol "wb")
		return Sl1Wb
	<|>
	do	try (skipSymbol "p2so-yo-k1")
		return P2so_yo_k1
	<|>
	do	try (skipSymbol "p3so-k1-yo-k1")
		return P3so_k1_yo_k1
	<|>
	do	try (skipSymbol "p3so-k1-yo-ssk")
		return P3so_k1_yo_ssk
	<|>
	do	try (skipSymbol "sl1-k1-yo-k1-psso")
		return Sl1_k1_yo_k1_psso
	<|>
		-- Combined knittels: Sl1_k1_yo_psso Sl1_k2_yo_psso
	do	try (
		string "sl"
		n1 <- num
		string "-"
		string "k"
		n2 <- num
		string "-"
		string "yo"
		string "-"
		skipSymbol "psso")
		return SlN_kN_yo_psso	n1 n2
	<|>
		-- Combined knittels: Sl1_k2_psso Sl1_k3_psso
	do	try (
		string "sl"
		n1 <- num
		string "-"
		string "k"
		n2 <- num
		string "-"
		skipSymbol "psso")
		return SlN_kN_psso	n1 n2
	<|>
		-- Combined knittels: Sl1_p2_psso Sl1_p3_psso
	do	try (
		string "sl"
		n1 <- num
		string "-"
		string "p"
		n2 <- num
		string "-"
		skipSymbol "psso")
		return SlN_pN_psso	n1 n2
	<|>
	do	try (skipSymbol "sl1-p3so-k2tog-yo-k1")
		return Sl1_p3so_k2tog_yo_k1
	<|>
		-- Combined knittels: Yo_k2_pyo Yo_k3_pyo
	do	try (
		string "yo"
		string "-"
		string "k"
		n1 <- num
		string "-"
		skipSymbol "pyo")
		return Yo_kN_pyo	n1
	<|>
		-- Combined knittels: Yo_p2_pyo Yo_p3_pyo
	do	try (
		string "yo"
		string "-"
		string "p"
		n1 <- num
		string "-"
		skipSymbol "pyo")
		return Yo_pN_pyo	n1
	<|>
		-- Combined knittels: 2_to_2Gather 2_to_3Gather 2_to_5Gather 2_to_7Gather 2_to_9Gather 3_to_2Gather 3_to_3Gather 3_to_5Gather 3_to_7Gather 3_to_9Gather 5_to_3Gather 5_to_5Gather 5_to_7Gather 5_to_9Gather 7_to_3Gather 7_to_5Gather 7_to_7Gather 7_to_9Gather
	do	try (
		n1 <- num
		string "-"
		string "to"
		string "-"
		n2 <- num
		
		skipSymbol "gather")
		return N_to_NGather	n1 n2
	<|>
		-- Combined knittels: Wrap2Sts Wrap3Sts Wrap4Sts Wrap5Sts Wrap6Sts Wrap7Sts Wrap8Sts Wrap9Sts
	do	try (
		skipSymbol "wrap"
		
		n1 <- num
		
		skipSymbol "sts")
		return WrapNSts	n1
	<|>
		-- Combined knittels: 1'1LeftThreadThru 2'2LeftThreadThru 3'3LeftThreadThru 4'4LeftThreadThru 5'5LeftThreadThru
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "left"
		
		skipSymbol "thread"
		
		skipSymbol "thru")
		return N'NLeftThreadThru	n1 n2
	<|>
		-- Combined knittels: 1'1PurledLeftThreadThru 2'2PurledLeftThreadThru 3'3PurledLeftThreadThru 4'4PurledLeftThreadThru 5'5PurledLeftThreadThru
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "purled"
		
		skipSymbol "left"
		
		skipSymbol "thread"
		
		skipSymbol "thru")
		return N'NPurledLeftThreadThru	n1 n2
	<|>
		-- Combined knittels: 1'1RightThreadThru 2'2RightThreadThru 3'3RightThreadThru 4'4RightThreadThru 5'5RightThreadThru
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "right"
		
		skipSymbol "thread"
		
		skipSymbol "thru")
		return N'NRightThreadThru	n1 n2
	<|>
		-- Combined knittels: 1'1PurledRightThreadThru 2'2PurledRightThreadThru 3'3PurledRightThreadThru 4'4PurledRightThreadThru 5'5PurledRightThreadThru
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "purled"
		
		skipSymbol "right"
		
		skipSymbol "thread"
		
		skipSymbol "thru")
		return N'NPurledRightThreadThru	n1 n2
	<|>
		-- Combined knittels: 1'1LC 1'2LC 1'3LC 2'1LC 2'2LC 2'3LC 3'1LC 3'2LC 3'3LC 4'1LC 4'2LC 4'3LC 4'4LC 5'5LC 6'6LC 7'7LC 8'8LC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "LC")
		return N'NLC	n1 n2
	<|>
		-- Combined knittels: 1'1LPC 1'2LPC 1'3LPC 2'1LPC 2'2LPC 2'3LPC 3'1LPC 3'2LPC 3'3LPC 4'1LPC 4'2LPC 4'3LPC 4'4LPC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "LPC")
		return N'NLPC	n1 n2
	<|>
		-- Combined knittels: 1'1LT 1'2LT 2'1LT
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "LT")
		return N'NLT	n1 n2
	<|>
		-- Combined knittels: 1'1LPT 1'2LPT 2'1LPT
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "LPT")
		return N'NLPT	n1 n2
	<|>
		-- Combined knittels: 1'1LSC 1'2LSC 1'3LSC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "LSC")
		return N'NLSC	n1 n2
	<|>
	do	try (skipSymbol "1/1"
		skipSymbol "LSAC")
		return 1'1LSAC
	<|>
		-- Combined knittels: 1'1'1LC 1'2'1LC 2'1'2LC 2'2'2LC 3'1'3LC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		string "/"
		n3 <- num
		
		skipSymbol "LC")
		return N'N'NLC	n1 n2 n3
	<|>
		-- Combined knittels: 1'1'1LPC 1'2'1LPC 2'1'2LPC 2'2'2LPC 3'1'3LPC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		string "/"
		n3 <- num
		
		skipSymbol "LPC")
		return N'N'NLPC	n1 n2 n3
	<|>
	do	try (skipSymbol "1/1/1"
		skipSymbol "LT")
		return 1'1'1LT
	<|>
	do	try (skipSymbol "1/1/1"
		skipSymbol "LPT")
		return 1'1'1LPT
	<|>
		-- Combined knittels: 1'1'1LCC 1'2'1LCC 2'1'2LCC 2'2'2LCC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		string "/"
		n3 <- num
		
		skipSymbol "LCC")
		return N'N'NLCC	n1 n2 n3
	<|>
		-- Combined knittels: 1'1RC 1'2RC 1'3RC 2'1RC 2'2RC 2'3RC 3'1RC 3'2RC 3'3RC 4'1RC 4'2RC 4'3RC 4'4RC 5'5RC 6'6RC 7'7RC 8'8RC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "RC")
		return N'NRC	n1 n2
	<|>
		-- Combined knittels: 1'1RPC 1'2RPC 1'3RPC 2'1RPC 2'2RPC 2'3RPC 3'1RPC 3'2RPC 3'3RPC 4'1RPC 4'2RPC 4'3RPC 4'4RPC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "RPC")
		return N'NRPC	n1 n2
	<|>
		-- Combined knittels: 1'1RT 1'2RT 2'1RT
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "RT")
		return N'NRT	n1 n2
	<|>
		-- Combined knittels: 1'1RPT 1'2RPT 2'1RPT
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "RPT")
		return N'NRPT	n1 n2
	<|>
		-- Combined knittels: 1'1RSC 1'2RSC 1'3RSC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		
		skipSymbol "RSC")
		return N'NRSC	n1 n2
	<|>
	do	try (skipSymbol "1/1"
		skipSymbol "RSAC")
		return 1'1RSAC
	<|>
		-- Combined knittels: 1'1'1RC 1'2'1RC 2'1'2RC 2'2'2RC 3'1'3RC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		string "/"
		n3 <- num
		
		skipSymbol "RC")
		return N'N'NRC	n1 n2 n3
	<|>
		-- Combined knittels: 1'1'1RPC 1'2'1RPC 2'1'2RPC 2'2'2RPC 3'1'3RPC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		string "/"
		n3 <- num
		
		skipSymbol "RPC")
		return N'N'NRPC	n1 n2 n3
	<|>
	do	try (skipSymbol "1/1/1"
		skipSymbol "RT")
		return 1'1'1RT
	<|>
	do	try (skipSymbol "1/1/1"
		skipSymbol "RPT")
		return 1'1'1RPT
	<|>
		-- Combined knittels: 1'1'1RCC 1'2'1RCC 2'1'2RCC 2'2'2RCC
	do	try (
		n1 <- num
		string "/"
		n2 <- num
		string "/"
		n3 <- num
		
		skipSymbol "RCC")
		return N'N'NRCC	n1 n2 n3
	<|>
	do	try (skipSymbol "k1"
		skipSymbol "below")
		return K1Below
	<|>
	do	try (skipSymbol "p1"
		skipSymbol "below")
		return P1Below



yarnPlacement :: Parser YarnPlacement
yarnPlacement =
    do  skipSymbol "wyif"
        return Wyif
    <|>
    do  skipSymbol "wyib"
        return Wyib


side :: Parser Side
side =
    do  parens (skipSymbol "RS")
        return R
    <|>
    do  parens (skipSymbol "WS")
        return W
    <|>
    do  return None