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

{-
getEditableLine :: String -> IO String
getEditableLine acc = do
    c <- getChar
    if c == '\n'
        then
            return $ acc ++ [c]
        else
            if c == '\b'
                then
                    getEditableLine
-}

eval :: String -> String
eval input = case P.parse expr "lipl" input of
    Left err -> show err
    Right val -> "==> " ++ show val

data LiplVal = Name String
    | List [LiplVal]
    | Integer Integer
    | Operator String
    | Expr [LiplVal]
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
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ List val

parseToken :: P.Parser LiplVal
parseToken = parseName <|> parseList <|> parseInteger <|> parseOperator

parseExpr :: P.Parser LiplVal
parseExpr = do
    val <- P.sepEndBy parseToken mustSpaces
    P.eof
    return $ Expr val

parseInteger :: P.Parser LiplVal
parseInteger = do
    val <- P.many1 P.digit
    return $ Integer (read val)

mustSpaces :: P.Parser ()
mustSpaces = P.skipMany1 P.space

test = do
    c <- getChar
    putStr $ " " ++ [c]
    hFlush stdout
    test
