module KSParser (ParseError, parseString) where
import General ( lexeme, parens, skipSymbol, symbol, num)
import KSSyntax
import Prelude hiding (lines) -- NOTE: Kan være lurt å ikke hide dem og heller bytte navn ? 
import Text.ParserCombinators.Parsec hiding (Line)
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.List (singleton)
import KnittelParser 


parseString :: String -> Either ParseError Pattern 
parseString s =
    case parse patternParser "" s of
        Left  error -> Left error
        Right e    -> return e
    where
        patternParser =
            do lexeme (return ())
               e <- pattern
               eof
               return e


pattern :: Parser Pattern
pattern = do lines


lines :: Parser [Line]
lines = many line


line :: Parser Line
line =
    do  c  <- course
        skipSymbol ":"
        is <-  instructions
        skipSymbol "."
        return (Course c is)


course :: Parser Course
-- NOTE: kan sikkert forenkles mye 
{- string "ro" også bestemme basert på ws, w, unds, und ? 
evt kombinere to og to? -}
course =
    try (
    do  skipSymbol "Rows"
        r <- numbs
        Row r <$> side)
    <|>
    try (
    do  skipSymbol "Row"
        r <- numbs
        Row r <$> side)
    <|>
    try (
    do  skipSymbol "Rounds"
        Round <$> numbs)
    <|>

    do  skipSymbol "Round"
        Round <$> numbs


instructions :: Parser Instructions
instructions = instruction `sepBy` try (symbol "," >> notFollowedBy (symbol "repeat"))


-- TODO: instructions
instruction :: Parser Instruction
instruction =
    loop
    <|>
    rep
    <|>
    Knittel <$> knittel


subinstructions :: Parser Instructions
subinstructions = (rep <|> (Knittel <$> knittel)) `sepBy` try (symbol "," >> notFollowedBy (symbol "repeat"))



loop :: Parser Instruction
loop =
    do  skipSymbol "*"
        subs <- subinstructions
        skipSymbol ","
        skipSymbol "repeat"
        skipSymbol "from"
        skipSymbol "*"
        Loop subs <$> end


rep :: Parser Instruction
rep =
    do  skipSymbol "["
        si <- subinstructions
        skipSymbol "]"
        Rep si <$> times


times :: Parser Times
times =
    do  skipSymbol "twice"
        return 2
    <|>
    do  n <- nzNum
        skipSymbol "times"
        return n


knittels :: Parser Instruction
knittels = Knittels <$> knittel `sepBy` symbol ","

    where sep = do  skipSymbol ","
                    notFollowedBy (skipSymbol "repeat" <|> skipSymbol "[")
                    return ()


-- TODO: Knittel! 
{-
knittel :: Parser Knittel
knittel =
    try (lexeme (
    do  _ <- string "k" <|> string "K"
        n <- num
        notFollowedBy (symbol "tog")
        return (K n)))
        {-
        choice [ (symbol "tog")  $> KNtog ,
            notFollowedBy (symbol "tog") $> K
        ]
        -- NOTE: option parseren? 
        try ( lexeme (string "tog"
        return KNtog n)) 
        -- TODO: Finn ut av hvordan vi skiller mellom k2tog og k2
            vi bruker try-}
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
        Slip n <$> yarnPlacement-}

{-
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
    -}


end :: Parser EndSts
end =
    do  skipSymbol "to"
        skipSymbol "last"
        sts
    <|>
    do return 0
    where sts = do  n <- nzNum
                    skipSymbol "sts"
                    return n
                <|>
                do  skipSymbol "st"
                    return 1

-- Numbs
numbs :: Parser LineNums
numbs = nums `chainl1` separator
    where separator = (try (symbol "," >> symbol "and") <|> symbol "," <|> symbol "and")   $> (++)

-- nums
nums :: Parser LineNums -- lager lister
nums = try (do  ds <- num
                notFollowedBy (symbol "-")
                return $ singleton ds)
    <|> do  x <- num
            skipSymbol "-"
            y <- num
            return [x..y]

-- nzNum
nzNum :: Parser Integer
nzNum =
    do  skipSymbol "0"
        unexpected "0 is not a valid number of times"
    <|>
    num
