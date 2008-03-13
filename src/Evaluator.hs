module Evaluator where

import qualified Control.Monad.Reader as R
--import qualified Control.Monad.Identity as M
import qualified Control.Monad.Error as E
import qualified Data.Map as Map
import Data.Maybe

import qualified Text.ParserCombinators.Parsec as P
import Parser (parse, parseSingle, parseMultiple)
import LangData
import CoreLib

import Debug.Trace (trace)

interpretSingle :: String -> String
interpretSingle input = case runEvalVal emptyEnv $ parseSingle input of
    Left err -> show err
    Right val -> case runEvalVal emptyEnv $ eval val of
        Left error -> show error
        Right value -> show value

interpretMultiple input = case runEvalVal emptyEnv $ parseMultiple input of
    Left err -> show err
    Right vals -> case mapM ((runEvalVal emptyEnv) . eval) vals of
        Left error -> show error
        Right values -> show values


    --Right val -> (show . unpackVal) $ eval val
    --Left err -> show err
    --Right val -> show (eval val)
    --Right expr@(Expr val) -> show (fst . pop . fst $ eval' [] val)
    --Right val -> show (eval val)
    --Right expr@(Expr val) -> show (expr, eval' [] val)
    --Right (TopLevel es) -> "==> " ++ show (evals es)
    --Right val -> "==> " ++ show (runEval Map.empty (eval val))



eval :: Val -> EvalVal Val
eval e@(Ident var) = do
    env <- R.ask
    case Map.lookup var env of
        Nothing -> E.throwError $ UnboundIdentErr "var not bound" var
        Just val -> return val
eval e@(Int _) = return e
eval e@(Float _) = return e
eval e@(Bool _) = return e
eval e@(Char _) = return e
eval e@(Str _) = return e
eval (List xs) = do          -- eager evaluation
    elems <- mapM eval xs
    return $ List elems
eval (Expr []) = return Null
eval (Expr [Expr expr]) = eval $ Expr expr -- hack for ((+ 1 2))
eval (Expr [Ident "=", Ident name, body]) = do
    env <- R.ask
    return $ Fun env name [] body
eval (Expr [Ident "if", pred, ifCase, elseCase]) = do
    ifOrElse <- eval pred
    case ifOrElse of
        Bool False -> eval elseCase
        otherwise -> eval ifCase
eval (Expr (Ident fname : args)) = do
    params <- mapM eval args -- eager evaluation
    funcall fname params
{-
eval (Expr (fn@(Ident fname) : args)) = do
    f <- eval fn
    case f of
        Fun env name keys body -> do
            params <- mapM eval args -- eager evaluation
            let env' = putKeyVals keys params env
            R.local (const env') (eval body)
        otherwise -> E.throwError $ NotFunErr "not function" fname
-}

eval (Expr [a]) = eval a
eval x = return x
--eval x = E.throwError $ BadExprErr "Bad Expr" x

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

