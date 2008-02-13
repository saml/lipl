module Main where

import System.IO
import System.Environment (getArgs)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import Data.Char (isSpace)
import qualified Data.Sequence as S
import Data.Sequence ((|>), (<|))
import qualified Data.Foldable as Foldable
import Debug.Trace (trace)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    repl

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

mydrop :: Int -> S.Seq a -> S.Seq a
mydrop i s
    | i < 0 = error $ "Can't drop " ++ show i ++ " elements"
    | i > S.length s = error "Can't drop more elements than I have."
    | otherwise = S.drop i s

myindex :: S.Seq a -> Int -> a
myindex s i
    | i >= S.length s = error "Index out of bound"
    | otherwise = S.index s i

exprToList (Expr l) = l

postfix :: [LiplVal] -> [LiplVal] -> [LiplVal]
postfix prev [] = prev
postfix prev rest@(r:rs) = let {
    curr = r;
    prev_len = length prev;
    rest_len = length rest;
} in case curr of
    Operator op -> if prev_len >= 2
        then let {
            rev = reverse prev;
            param2 = head rev;
            param1 = (head . tail) rev;
        } in postfix (prev ++ [List [param1, param2, curr]])
                     rs
        else
            if prev_len >= 1
                then let {
                    param1 = (head . tail) prev;
                    param2 = head rs;
                } in postfix (prev ++ [List [param1, param2, curr]])
                             (tail rs)
                else let {
                    param1 = head rs;
                    param2 = (head . tail) rs;
                } in postfix (prev ++ [List [param1, param2, curr]])
                             (tail (tail rs))
    --List l -> postfix (prev ++ [List (postfix [] l)]) rs
    otherwise -> postfix (prev ++ [curr]) rs


toPostfix :: S.Seq LiplVal -> S.Seq LiplVal -> S.Seq LiplVal
toPostfix prev rest
    | S.null rest = prev
    | otherwise = let {
        curr = myindex rest 0;
        prev_len = S.length prev;
        rest_len = S.length rest;
} in case curr of
    Operator op -> if prev_len >= 2 -- both params exist (assume binary op)
        then trace ("|" ++ op ++ "|") $ toPostfix (prev |> (Operator op))
                       (mydrop 1 rest)
        else
            if prev_len >= 1
                then
                    let param = myindex rest 1
                    in trace ("|" ++ show param ++ " " ++ op ++ "|") $ toPostfix ((prev |> param) |> Operator op)
                                 (mydrop 2 rest)
                else
                    let param1 = myindex rest 1
                        param2 = myindex rest 2
                    in trace ("|" ++ show param1 ++ " " ++ show param2 ++ " " ++ op ++ "|") $ toPostfix (prev |> param1 |> param2 |> Operator op)
                                 (mydrop 3 rest)
    List l -> toPostfix (prev |>
        seqToList (toPostfix S.empty (S.fromList l)))
        (mydrop 1 rest)
    otherwise -> toPostfix (prev |> curr) (mydrop 1 rest)



foo = "+ 1 (1 + (2 3 -)) - 4"
--bar = toPostfix S.empty (S.fromList (toList (eval' foo)))
bar = postfix [] (toList (eval' foo))

test = do
    putStrLn $ eval "+ 1 (1 + (2 3 -)) - 4"
    putStrLn $ eval "a = (+ (1 :: Float) 2)"
    putStrLn $ eval "foo = (1 + 2)"
    putStrLn $ eval "= foo (+ 1 2)"
    putStrLn $ eval "foo 1 2 + ="
    --putStrLn $ show (toPostfix S.empty (S.fromList (toList (eval "= foo (+ 1 2)"))))

