module General where
import Text.ParserCombinators.Parsec
import Data.Functor (void)
import Data.Char (isDigit, toUpper, toLower)

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
skipSymbol [] = return ()
skipSymbol (s: ss) =  lexeme $ 
    do  _ <- try (string (toLower s: ss)) <|> string (toUpper s : ss)
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