module Main where

import System.IO
import System.Environment (getArgs)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import Data.Char (isSpace)
import qualified Control.Monad.Reader as M
import qualified Control.Monad.Identity as M
import qualified Control.Monad.Error as M
import qualified Data.Map as Map
import Data.Maybe
--import qualified Data.Sequence as S
--import Data.Sequence ((|>), (<|))
--import qualified Data.Foldable as Foldable
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
            putStrLn $ interpret (trim input)
            repl

prompt :: String -> IO String
prompt p = do
    putStr p
    hFlush stdout
    getLine

trim :: String -> String
trim = f . f where
    f = reverse . dropWhile isSpace

interpret :: String -> String
interpret input = case P.parse parseExpr "lipl" input of
    Left err -> show err
    Right val -> "==> " ++ show (runEval Map.empty (eval val))

--builtIns :: Map.Map String Val
--builtIns = Map.fromList [("+", FunVal]

-- Parser

type Name = String
data Expr = Ident Name
--    | Op Name
    | Int Integer
--    | Funcall { name :: Name, args :: [Expr] }
--    | Fundef { name :: Name, args :: [Expr], body :: Expr }
--    | ParenExpr [Expr]
    | Expr [Expr]
    deriving (Show)

identChar :: P.Parser Char
identChar = P.letter <|> P.digit <|> P.oneOf "_-"

parseIdent :: P.Parser Expr
parseIdent = parseOp <|> parseName where
    parseOp = do
        op <- P.string "+" <|> P.string "-"
        return $ Ident op
    parseName = do
        first <- P.letter
        rest <- P.many identChar
        return $ Ident (first : rest)

{-
parseOp :: P.Parser Expr
parseOp = do
    val <- P.string "="
        <|> P.string "+" <|> P.string "-"
    return $ Op val
-}

parseInt :: P.Parser Expr
parseInt = do
    sign <- P.try (P.option "" (P.string "-"))
    val <- P.many1 P.digit
    return $ Int (read $ sign ++ val)

{-
parseFundef :: P.Parser Expr
parseFundef = do
    P.string "def"
    mustSpaces
    Ident name <- parseIdent
    mustSpaces
    ParenExpr args <- parseParenExpr
    mustSpaces
    Expr (body:_) <- parseExpr
    return $ Fundef name args body
-}

lparen = P.char '(' >> P.spaces

rparen = P.spaces >> P.char ')'

parseParenExpr :: P.Parser Expr
parseParenExpr = do
    lparen
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ Expr val

parseExpr :: P.Parser Expr
parseExpr = do
    val <- P.sepEndBy parseToken mustSpaces
    P.eof
    return $ Expr val

parseToken :: P.Parser Expr
parseToken = -- parseFundef
    P.try parseParenExpr
    <|> P.try parseInt
    <|> P.try parseIdent
--    <|> parseOp

mustSpaces :: P.Parser ()
mustSpaces = P.skipMany1 P.space

-- Evaluator

data Val = IntVal Integer
    | FunVal { name :: String, arity :: Int, env :: Env, body :: Expr }
    | NullVal
    deriving (Show)

type Env = Map.Map Name Val
type Evaluated a = M.ReaderT Env (M.ErrorT String M.Identity) a

runEval :: Env -> Evaluated a -> Either String a
runEval env e = M.runIdentity (M.runErrorT (M.runReaderT e env))

eval :: Expr -> Evaluated Val
eval (Int i) = return $ IntVal i
eval (Ident n) = do
    env <- M.ask
    case Map.lookup n env of
        Nothing -> M.throwError ("unbound identifier: " ++ n)
        Just val -> return val
eval (Expr []) = return NullVal
--eval (Expr (x:xs)) = do
--    f <- eval x

eval a = M.throwError $ "todo: " ++ show a

{-
eval expr@(Expr (f:args)) = case f of
    Ident "+" ->
        let
            Int arg1 = head args
            rest = tail args
            Int arg2 = head rest
            remainder = tail rest
        in
            IntVal (arg1 + arg2)
    Ident "-" ->
        let
            Int arg1 = head args
            Int arg2 = head (tail args)
        in
            IntVal (arg1 - arg2)
    _ -> error $ "no parse for: " ++ show expr
-}

{-
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
-}
