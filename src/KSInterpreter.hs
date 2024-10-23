module KSInterpreter where 

import KSSyntax ( Pattern )
import Knittels ()
import Control.Monad ()
import Control.Arrow (second)
import Data.List ()

type Output = [String]
-- TODO: redefiner
data RuntimeError =
    UnboundVariable String --VariableName
  | BadFunction     String --FunctionName
  | BadArgument     String --ErrorMessage
  deriving (Eq, Show)

execute :: Pattern -> (Output, Maybe RuntimeError)
execute pattern = undefined
    {- case run (exec pattern) [] of 
                      (Left  error, output) -> (output, Just error) 
                      (Right _    , output) -> (output, Nothing) -}