module Evaluator where

import qualified Control.Monad.Reader as M
import qualified Control.Monad.Identity as M
import qualified Control.Monad.Error as M
import qualified Data.Map as Map
import Data.Maybe

import qualified Text.ParserCombinators.Parsec as P
import Parser (parse)
import LangData
import CoreLib

import Debug.Trace (trace)

interpret :: String -> String
interpret input = case parse input of
    Left err -> show err
    Right expr@(Expr val) -> show (expr, eval' [] val)
    --Right (TopLevel es) -> "==> " ++ show (evals es)
    --Right val -> "==> " ++ show (runEval Map.empty (eval val))
{-
data Err = ParserErr P.ParseError
    | ArityErr Int [Expr]
    | Default String

showErr :: Err -> String
showErr (ParserErr err) = "Parse error at " ++ show err
showErr (ArityErr i ts) =
    "Expecting " ++ show i ++ " arguments; found " ++ unwordsToks ts

unwordsToks = unwords . map show

instance Show Err where show = showErr

instance M.Error Err where
    noMsg = Default "Error occurred"
    strMsg = Default

type ThrowErr = Either Err

data Val = IntVal Integer
    | FloatVal Double
--    | FunVal { ident :: String
--        , arity :: Int, env :: Env, body :: Parser.Expr }
    | BoolVal Bool
    | NullVal

showVal :: Val -> String
showVal (IntVal i) = show i
showVal (FloatVal f) = show f
showVal (BoolVal b) = show b
showVal NullVal = ""

instance Show Val where
    show = showVal
-}

eval' :: Stack -> Queue -> (Stack, Queue)
eval' s [] = (s, [])
eval' s (Ident fname : args) = let
    (s', q) = funcall' s args fname
    in
        eval' s' q

eval' s (x : q) = let
    s' = push x s
    in
        eval' s' q

funcall' :: Stack -> Queue -> String -> (Stack, Queue)
funcall' s q fname = case lookup fname primitives' of
    Nothing -> (push (Bool False) s, q)
    Just f -> f s q

primitives' :: [(String, Stack -> Queue -> (Stack, Queue))]
primitives' = [
    ("+", opAdd')
    ]

fromVal (Int i) = i
--fromVal (Float f) = f
toVal i = Int i

opAdd' :: Stack -> Queue -> (Stack, Queue)
opAdd' s q | length s >= 2 = let
    (a, s') = pop s
    (b, s'') = pop s'
    result = toVal $ fromVal a + fromVal b
    in
        (push result s'', q)

opAdd' s q | length s >= 1 = let
    (a, s') = pop s
    (b, q') = front q
    b' = case b of
        Expr e -> (fst . pop . fst) (eval' [] e)
        otherwise -> b
    result = toVal $ fromVal a + fromVal b'
    in
        (push result s', q')

opAdd' s q = let
    (a, q') = front q
    (b, q'') = front q'
    a' = case a of
        Expr e -> (fst . pop . fst) (eval' [] e)
        otherwise -> a
    b' = case b of
        Expr e -> (fst . pop . fst) (eval' [] e)
        otherwise -> b
    result = toVal $ fromVal a' + fromVal b'
    in
        (push result s, q'')





eval :: Val -> Val
eval (Expr []) = Null
eval (Expr [Expr expr]) = eval $ Expr expr -- hack for ((+ 1 2))
eval (Expr (Ident fname : args)) = funcall fname $ map eval args
eval x = x

funcall :: String -> [Val] -> Val
funcall fname args = case lookup fname primitives of
    Nothing -> Bool False
    Just f -> f args

-- [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/)), ("div", div)] :: (Num a) => [(String, a)]

{-
type Env = Map.Map String Val
type Evaluated a = M.ReaderT Env (M.ErrorT String M.Identity) a

runEval :: Env -> Evaluated a -> Either String a
runEval env e = M.runIdentity (M.runErrorT (M.runReaderT e env))

eval :: Parser.Expr -> Evaluated Val
eval (Parser.Int i) = return $ IntVal i
eval (Parser.Ident n) = do
    env <- M.ask
    case Map.lookup n env of
        Nothing -> M.throwError ("unbound identifier: " ++ n)
        Just val -> return val
eval (Parser.Expr []) = return NullVal
eval (Parser.TopLevel (x:xs)) = eval x
eval (Parser.Expr (Parser.Ident fname : args)) =
    funcall fname $ map eval args
eval a = M.throwError $ "todo: " ++ show a


funcall :: String -> [Evaluated Val] -> Evaluated Val
funcall fname args = maybe
    (return $ BoolVal False) ($ args) $ lookup fname primitives

primitives :: [(String, [Evaluated Val] -> Evaluated Val)]
primitives = [
    ("+", numBinOp (+))
    , ("-", numBinOp (-))
    , ("*", numBinOp (*))
    ]

numBinOp :: (Integer -> Integer -> Integer) -> [Evaluated Val] -> Evaluated Val
numBinOp op (arg1:arg2:xs) = do
    a <- arg1
    b <- arg2
    return $ IntVal $ (unpackInt a `op` unpackInt b)

unpackInt :: Val -> Integer
unpackInt (IntVal i) = i
-}

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

