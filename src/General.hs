module General where
import Text.ParserCombinators.Parsec
import Data.Char (isDigit)
import Data.Functor (void)

squigly     :: Parser a -> Parser a
squigly     =  between (skipSymbol "{") (skipSymbol "}")
brackets    :: Parser a -> Parser a 
brackets    =  between (skipSymbol "[")  (skipSymbol "]")
parens      :: Parser a -> Parser a
parens      =  between (skipSymbol "(") (skipSymbol ")")
symbol      :: String -> Parser String
symbol c    =  lexeme (string c) 
lexeme      :: Parser a -> Parser a
lexeme p    =  p <* many (void space <|> comment)

skipSymbol   :: String -> Parser ()
skipSymbol s =  lexeme $ 
    do  _ <- string s
        return ()


num :: Parser Integer
num = do ds <- lexeme $ many1 $ satisfy isDigit
         return (read ds)


comment :: Parser ()
comment = 
    try (
        do  skipSymbol "(" 
            _ <- manyTill anyChar (skipSymbol ")")
            return ()
    )