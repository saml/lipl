module Evaluator where

import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Fix as F
import qualified Control.Exception as Ex
import qualified Data.Map as Map

import Parser
import LangData
import CoreLib
import EvalUtils
import Utils
import EvalMonad
import Error

import Debug.Trace (trace)

--eval :: (MonadEval m, E.MonadError Err m, T.MonadIO m)
--    => Val -> m Val
eval e@(Int _) = return e
eval e@(Float _) = return e
eval e@(Bool _) = return e
eval e@(Char _) = return e
eval e@(Str _) = return e

eval e@(Ident var) = do
    val <- getVal var
    eval val

eval (List xs) = do
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
    env <- getEnvFor (unboundVars e)
    env' <- getEnv
    --traceM ("lambda: " ++ show e)
    --traceM ("lambda env: " ++ show env)
    --traceM ("lambda env': " ++ show env')
    let fun = Fun env args body
    --traceM ("lambda fun: " ++ show fun)
    return fun

eval e@(FunDef name args body) = do
    env <- getEnvFor (unboundVars e)
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

eval e@(Let env body) = do
    env' <- runLocally (keyValToEnv env)
    --traceM ("let env: " ++ show env')
    withEnv env' (eval body)

eval (Fun env [] body) = do
    traceM ("fun: env: " ++ show env)
    traceM ("fun: body: " ++ show body)
    withEnv env (eval body)

eval (Closure env body) = withEnv (Map.fromList env) (eval $ body)

eval (Expr []) = return Null

eval (Expr [e]) = eval e -- unwrap outer parens

{-
eval (Expr [Ident "=", Ident name, body]) = do
    putVal name body
    env <- getEnvFor (unboundVars body)
    return $ Fun env [] body

eval (Expr (Ident "free-vars" : e)) = do
    return $ List (map Ident (unboundVars (Expr e)))

eval (Expr (Ident "f-v" : e)) = do
    e' <- eval $ Expr e
    return $ List (map Ident (unboundVars e'))
-}

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



runLocally action = do
    prev <- getEnvs
    pushEnv emptyEnv
    result <- action
    putEnvs prev
    return result

keyValToEnv ((k,v):xs) = do
    v' <- eval v
    putVal k v'
    keyValToEnv xs
keyValToEnv [] = do
    env <- getEnv
    return env


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
        val <- action
        popEnv
        return val)
    `E.catchError`
    (\e -> do
        popEnv
        E.throwError e)


--apply :: Val -> Val -> m Val
apply (Fun env (arg:rst) body) e = do
    let env' = Map.insert arg e env
    if null rst
        then
            withEnv env'(eval body)
        else
            return $ Fun env' rst body
apply e _ = E.throwError $ NotFunErr "not function" (show e)

evalWith env val = withEnv env (eval val)

test s = case parseSingle s of
    Right val -> unboundVars val

