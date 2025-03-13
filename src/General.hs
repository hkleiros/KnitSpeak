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
symbol c    =  string c <* many space
lexeme      :: Parser a -> Parser a
lexeme p    =  p <* many (void space <|> comment)

skipSymbol   :: String -> Parser ()
skipSymbol [] = return ()
skipSymbol (s: ss) =  lexeme $ 
    do  _ <- try (string (toLower s: ss)) <|> string (toUpper s : ss)
        return ()


num :: Parser Int
num = do ds <- many1 $ satisfy isDigit <* many space
         return (read ds)


comment :: Parser ()
comment = 
    try (
        do  skipSymbol "(" 
            _ <- manyTill anyChar (skipSymbol ")")
            return ()
    )