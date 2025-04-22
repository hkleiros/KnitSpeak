module KSParser (ParseError, parseString) where

import Prelude hiding (lines)
import General 
    ( lexeme,
      skipSymbol,
      symbol,
      num,
      parens,
      brackets,
      squigly,
      comment)
import KSSyntax
    ( Pattern(..),
      Line(..),
      Course(..),
      Instructions,
      Instruction(..),
      LineNums,
      Pattern,
      EndSts,
      Times,
      Side (..) )
import Text.Parsec
    ( chainl1,
      eof,
      notFollowedBy,
      sepBy,
      (<|>),
      many,
      parse,
      unexpected,
      ParseError,
      --Parser,
      try,
      optional )
import Text.Parsec.String (Parser)
import Data.Functor (($>), void)
import Data.List (singleton)
import KnittelParser (knittel, tbl)
import Knittels (Knittel(..), KName (Knit, Purl), KArity (..))

parseString :: String -> Either ParseError Pattern
parseString s =
    case parse patternParser "" s of
        Left e -> Left e
        Right p -> return p
    where
        patternParser =
            do lexeme (return ())
               e <- pattern
               eof
               return e


pattern :: Parser Pattern
pattern = do Pattern <$> many course


course :: Parser Course
course =
    try (
    do  l  <- line
        skipSymbol ":"
        is <-  instructions
        c  <- comment <|> return ""
        optional (void (symbol "."))
        return (Course l is c))
    <|>
    try (
    do  skipSymbol "Repeat"
        r <- try (symbol "rows") <|> try (symbol "rounds") <|> try (symbol "row") <|> try (symbol "round")
        ln <- nums
        t <- try times <|> return 0 
        void (symbol ".")
        return (MultilineRepeat r ln t))
    <|> 
    do  Comment <$> comment


line :: Parser Line
line =
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
        r <- numbs
        Round r <$> side)
    <|>
    do  skipSymbol "Round"
        r <- numbs
        Round r <$> side

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
        Loop [Knittel (KInst Knit 0 (KArity (-1)) t)] <$> end )
    <|>
    try (
    do  skipSymbol "Purl"
        t <- tbl
        Loop [Knittel (KInst Purl 0 (KArity (-1)) t)] <$> end )



rep :: Parser Instruction
rep =
    do  si <- brackets subinstructions
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

nzNum :: Parser Int
nzNum =
    do  skipSymbol "0"
        unexpected "0 is not a valid number of times"
    <|>
    num


side :: Parser Side
side =
    do  m <- parens s <|> brackets s <|> squigly s
        case m of
            "RS" -> return R
            "WS" -> return W
            _    -> return None -- Will not ever be used but patternmatching
    <|>
    do  return None

    where   s = lexeme  (symbol "RS") <|> lexeme (symbol "WS")