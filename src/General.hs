module General
    ( squigly,
     between,
     brackets,
     parens,
     symbol,
     lexeme,
     skipSymbol,
     num,
     maybeINum,
     comment)
where
import Text.Parsec
    ( anyChar,
      satisfy,
      string,
      between,
      manyTill,
      (<|>),
      many1,
      try,
      optional,
      spaces)
import Text.Parsec.String (Parser)
import Data.Char (isDigit, toUpper, toLower)

squigly     :: Parser a -> Parser a
squigly     =  between (skipSymbol "{") (skipSymbol "}")
brackets    :: Parser a -> Parser a
brackets    =  between (skipSymbol "[")  (skipSymbol "]")
parens      :: Parser a -> Parser a
parens      =  between (skipSymbol "(") (skipSymbol ")")
symbol      :: String -> Parser String
symbol c    =  string c <* spaces
lexeme      :: Parser a -> Parser a
lexeme p    =  p <* spaces

skipSymbol   :: String -> Parser ()
skipSymbol [] = return ()
skipSymbol (s: ss) =  lexeme $
    do  _ <- try (string (toLower s: ss)) <|> string (toUpper s : ss)
        return ()

num :: Parser Int
num = do ds <- many1 (satisfy isDigit) <* spaces
         return (read ds)

maybeINum :: Parser Int
maybeINum = try ( num <* optional (try (skipSymbol "sts") <|> skipSymbol "st") )
            <|> return 1

comment :: Parser String
comment = do  skipSymbol "("
              manyTill anyChar (skipSymbol ")")