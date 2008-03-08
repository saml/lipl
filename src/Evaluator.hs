module Evaluator where

--import qualified Control.Monad.Reader as M
--import qualified Control.Monad.Identity as M
import qualified Control.Monad.Error as E
import qualified Data.Map as Map
import Data.Maybe

import qualified Text.ParserCombinators.Parsec as P
import Parser (parse, parseSingle)
import LangData
import CoreLib

import Debug.Trace (trace)

interpret :: String -> String
interpret input = case parseSingle input of
    Left err -> show err
    Right val -> case eval val of
        Left error -> show error
        Right value -> show value

    --Right val -> (show . unpackVal) $ eval val
    --Left err -> show err
    --Right val -> show (eval val)
    --Right expr@(Expr val) -> show (fst . pop . fst $ eval' [] val)
    --Right val -> show (eval val)
    --Right expr@(Expr val) -> show (expr, eval' [] val)
    --Right (TopLevel es) -> "==> " ++ show (evals es)
    --Right val -> "==> " ++ show (runEval Map.empty (eval val))

eval :: Val -> CanBeErr Val
eval e@(Int _) = return e
eval e@(Float _) = return e
eval e@(Bool _) = return e
eval (Expr []) = return Null
eval (Expr [Expr expr]) = eval $ Expr expr -- hack for ((+ 1 2))
eval (Expr (Ident fname : args)) = do
    params <- mapM eval args
    funcall fname params
eval x = E.throwError $ BadExprErr "Bad Expr" x

funcall :: String -> [Val] -> CanBeErr Val
funcall fname args = case lookup fname primitives of
    Nothing -> E.throwError
        $ NotFunErr "Unrecognizable primitive function" fname
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

