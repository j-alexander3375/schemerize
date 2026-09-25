module Main (main) where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char 
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
        Left err -> "No match: " ++ show err
        Right val -> "Found Value"




main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
