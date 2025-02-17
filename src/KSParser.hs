module KSParser (ParseError, parseString) where
import General ( lexeme,  skipSymbol, symbol, num)
import KSSyntax
    ( Line(..),
      Course(..),
      Instructions,
      Instruction(..),
      LineNums,
      Pattern,
      EndSts,
      Times )
import Prelude hiding (lines) -- NOTE: Kan være lurt å ikke hide dem og heller bytte navn ? 
import Text.ParserCombinators.Parsec hiding (Line)
import Data.Functor (($>))
import Data.List (singleton)
import KnittelParser ( side, knittel, tbl)
import Knittels (Knittel(..), KName (Knit, Purl), KArity (..))


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
    <|>
    try (
    do  skipSymbol "Knit"
        t <- tbl
        Loop [Knittel (KInst Knit (KArity (-1)) t)] <$> end )
    <|>
    try (
    do  skipSymbol "Purl"
        t <- tbl
        Loop [Knittel (KInst Purl (KArity (-1)) t)] <$> end )



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


numbs :: Parser LineNums
numbs = nums `chainl1` separator
    where separator = (try (symbol "," >> symbol "and") <|> symbol "," <|> symbol "and")   $> (++)

nums :: Parser LineNums -- lager lister
nums = try (do  ds <- num
                notFollowedBy (symbol "-")
                return $ singleton ds)
    <|> do  x <- num
            skipSymbol "-"
            y <- num
            return [x..y]

nzNum :: Parser Integer
nzNum =
    do  skipSymbol "0"
        unexpected "0 is not a valid number of times"
    <|>
    num
