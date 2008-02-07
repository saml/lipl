module Lipl.Repl where

import System.IO (hFlush, stdout)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

repl :: IO ()
repl = do
    input <- prompt "lipl> "
    if input == ":q"
        then putStrLn "bye"
        else do
            putStrLn $ eval input
            repl

prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    getLine

eval :: String -> String
eval input = case P.parse expr "lipl" input of
    Left err -> show err
    Right val -> "==> " ++ show val

data LiplVal = Name String
    | List [LiplVal]
    | Integer Integer
    | Operator String
    deriving Show

expr :: P.Parser LiplVal
expr = parseExpr

nameChar :: P.Parser Char
nameChar = P.letter <|> P.digit <|> P.oneOf "_-"

parseName :: P.Parser LiplVal
parseName = do
    first <- P.letter
    rest <- P.many nameChar
    return $ Name (first : rest)

parseOperator :: P.Parser LiplVal
parseOperator = do
    val <- P.string "=" <|> P.string "+" <|> P.string "::" <|> P.string "-"
    return $ Operator val

lparen = P.char '(' >> P.spaces

rparen = P.spaces >> P.char ')'

parseList :: P.Parser LiplVal
parseList = do
    lparen
    val <- P.sepEndBy parseExpr P.spaces
    rparen
    return $ List val

parseExpr :: P.Parser LiplVal
parseExpr = parseName <|> parseList <|> parseInteger <|> parseOperator

parseInteger :: P.Parser LiplVal
parseInteger = do
    val <- P.many1 P.digit
    return $ Integer (read val)

