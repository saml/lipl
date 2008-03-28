module Evaluator where

import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Fix as F
import qualified Control.Exception as Ex
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List as List
import Data.List ((\\))
import Data.Traversable (traverse)
import qualified Text.ParserCombinators.Parsec as P
import Parser
import LangData
import CoreLib
import Utils

import Debug.Trace (trace)

eval e@(Ident var) = do
    val <- getVal var
    eval val
eval e@(Int _) = return e
eval e@(Float _) = return e
eval e@(Bool _) = return e
eval e@(Char _) = return e
eval e@(Str _) = return e

eval (List xs) = do          -- eager evaluation
    elems <- mapM eval xs
    return $ List elems

eval (PrimFun name) = do
    let arity = arityOf name
    let fun = Prim name arity []
    if arity == 0
        then
            eval fun
        else
            return fun

eval (Prim name 0 []) = funcall name []

eval (Lambda [] body) = eval body

eval e@(Lambda args body) = do
    env <- getEnv --For (freeVars e)
    let fun = Fun env args body
    return fun


eval e@(FunDef name args body) = do
    env <- getEnv --For (freeVars e)
    putVal name (Fun env args body) -- for recursive definition ??
    env' <- getEnv
    let fun = Fun env' args body
    updateVal name fun
    return fun

eval (If pred ifCase elseCase) = do
    ifOrElse <- eval pred
    case ifOrElse of
        Bool False -> eval elseCase
        otherwise -> eval ifCase

eval (Let env body) = do
    currEnv <- getEnv --For (freeVars body)
    env' <- evalEnv (env `Map.union` currEnv)
    withEnv env' (eval body)

eval (Fun env [] body) = withEnv env (eval body)

eval (Closure env body) = withEnv env (eval $ body)

eval (Expr []) = return Null
eval (Expr [e]) = eval e -- unwrap outer parens
eval (Expr [Ident "=", Ident name, body]) = do
    putVal name body
    env <- getEnv --For (freeVars body)
    return $ Fun env [] body

eval (Expr (f:e)) = do -- both f and e are Val because Expr [e] case
    let firstArg = head e
    let restArgs = tail e
    function <- eval $ f
    case function of
        fun@(Fun env _ _) -> do
            arg1 <- eval $ firstArg
            evalFun env fun arg1 restArgs
        fun@(Prim name remaining args) -> do
            arg1 <- eval $ firstArg
            evalPrim name arg1 restArgs args remaining
        otherwise -> E.throwError $ NotFunErr "" (show f)

eval x = return x

evalFun env fun arg1 restArgs = do
    withEnv env (do
        partial <- apply fun arg1
        eval $ Expr (partial : restArgs))

evalPrim fname arg1 restArgs argsHave remaining = do
    let args = argsHave ++ [arg1]
    if remaining == 1
        then
            funcall fname args
        else
            eval $ Expr (
                (Prim fname (remaining-1) args) : restArgs)

withEnv env action = do
    (do
        pushEnv env
        --extendPushEnv env
        val <- action
        popEnv
        return val)
    `E.catchError`
    (\e -> do
        popEnv
        E.throwError e)


apply :: Val -> Val -> Wrap Val
apply (Fun env (arg:rst) body) e = do
    let env' = Map.insert arg e env
    if null rst
        then
            withEnv env'(eval body)
        else
            return $ Fun env' rst body
apply e _ = E.throwError $ NotFunErr "not function" (show e)

evalEnv env = do
    env' <- withEnv env (traverse eval env)
    return env'

toClosure e = do
    env <- getEnv --For (freeVars e)
    return $ Closure env e

test s = case parseSingle s of
    Right val -> freeVars val
