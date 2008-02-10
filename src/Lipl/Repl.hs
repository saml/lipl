module Lipl.Repl where

import System.IO (hFlush, stdout)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import Data.Char (isSpace)
import qualified Data.Sequence as S
import Data.Sequence ((|>), (<|))
import qualified Data.Foldable as Foldable

repl :: IO ()
repl = do
    input <- prompt "lipl> "
    if input == ":q"
        then putStrLn "bye"
        else do
            putStrLn $ eval (trim input)
            repl

prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    getLine

trim :: String -> String
trim = f . f where
    f = reverse . dropWhile isSpace

eval :: String -> String
eval input = case P.parse parseExpr "lipl" input of
    Left err -> show err
    Right val -> "==> " ++ show val

eval' :: String -> LiplVal
eval' input = case P.parse parseExpr "lipl" input of
    Left err ->  error "exit"
    Right val -> val

data LiplVal = Name String
    | List [LiplVal]
    | Integer Integer
    | Operator String
    | Expr [LiplVal]
    deriving (Show)

toList :: LiplVal -> [LiplVal]
toList (Expr a) = a
toList (List a) = a

nameChar :: P.Parser Char
nameChar = P.letter <|> P.digit <|> P.oneOf "_-"

parseName :: P.Parser LiplVal
parseName = do
    first <- P.letter
    rest <- P.many nameChar
    return $ Name (first : rest)

parseOperator :: P.Parser LiplVal
parseOperator = do
    val <- P.string "="
        <|> P.string "+" <|> P.string "::" <|> P.string "-"
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
parseToken = parseName
    <|> parseList <|> parseInteger <|> parseOperator

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

prettyPrint :: LiplVal -> String
prettyPrint (Name name) = name
prettyPrint (Operator op) = op
prettyPrint (Integer i) = show i
prettyPrint (List []) = ""
prettyPrint (List (x:xs)) = prettyPrint x ++ " " ++ prettyPrint (List xs)
prettyPrint (Expr []) = ""
prettyPrint (Expr (x:xs)) = prettyPrint x ++ " " ++ prettyPrint (Expr xs)


toStr :: S.Seq LiplVal -> String
toStr s = toStr' (Foldable.toList s)
toStr' :: [LiplVal] -> String
toStr' (x:xs) = prettyPrint x ++ " " ++ toStr' xs
toStr' [] = ""

seqToList :: S.Seq LiplVal -> LiplVal
seqToList s
    | S.null s = List []
    | otherwise = List (Foldable.toList s)


toPostfix :: S.Seq LiplVal -> S.Seq LiplVal -> S.Seq LiplVal
toPostfix prev rest
    | S.null rest = prev
    | otherwise = let
        curr = S.index rest 0
        prev_len = S.length prev
        rest_len = S.length rest
        in
            case curr of
                Operator op -> if prev_len >= 2
                    then toPostfix (prev |> (Operator op))
                                   (S.drop 1 rest)
                    else
                        if prev_len >= 1
                            then
                                let param = S.index rest 1
                                in toPostfix ((prev |> param)
                                                |> Operator op)
                                             (S.drop 2 rest)
                            else
                                let param1 = S.index rest 1
                                    param2 = S.index rest 2
                                in toPostfix (prev |> param1
                                                |> param2 |> Operator op)
                                             (S.drop 3 rest)
                List l -> toPostfix (prev |>
                    seqToList (toPostfix S.empty (S.fromList l)))
                    (S.drop 1 rest)
                otherwise -> toPostfix (prev |> curr) (S.drop 1 rest)



foo = "+ 1 (1 + (2 3 -)) - 4"
bar = toStr (toPostfix S.empty (S.fromList (toList (eval' "+ 1 (1 + (2 3 -)) - 4"))))

test = do
    putStrLn $ eval "+ 1 (1 + (2 3 -)) - 4"
    putStrLn $ eval "a = (+ (1 :: Float) 2)"
    putStrLn $ eval "foo = (1 + 2)"
    putStrLn $ eval "= foo (+ 1 2)"
    putStrLn $ eval "foo 1 2 + ="
    --putStrLn $ show (toPostfix S.empty (S.fromList (toList (eval "= foo (+ 1 2)"))))

