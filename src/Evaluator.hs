module Evaluator where

import qualified Control.Monad.Error as E

import qualified Data.Map as Map

import LangData
import CoreLib
import EvalUtils
import Utils
import EvalMonad
import PosMonad
import Error


--eval :: (MonadEval m, E.MonadError Err m, T.MonadIO m)
--    => Val -> m Val
eval e@(Int _) = return e
eval e@(Float _) = return e
eval e@(Bool _) = return e
eval e@(Char _) = return e
eval e@(Str _) = return e

eval (At pos e) = do
    setSourcePos pos
    eval e

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
    --env <- getEnvFor (unboundVars e)
    env' <- getEnv
    let fun = Fun env' args body
    return fun

eval e@(FunDef name args body) = do
    --env <- getEnvFor (unboundVars e)
    env <- getEnv
    putVal name (Fun env args body)
    env' <- getEnv
    let fun = Fun env' args body
    updateVal name fun
    return fun

eval e@(Seq e1 e2) = do
    eval e1
    eval e2

eval (If pred ifCase elseCase) = do
    ifOrElse <- eval pred
    case ifOrElse of
        Bool False -> eval elseCase
        otherwise -> eval ifCase

eval e@(Let env body) = do
    env' <- runLocally (keyValToEnv env)
    withEnv env' (eval body)

eval (Fun env [] body) = do
    withEnv env (eval body)

--eval (Closure env body) = withEnv (Map.fromList env) (eval $ body)

eval (Expr []) = return Null

eval (Expr [e]) = eval e -- unwrap outer parens


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
        otherwise -> do
            pos <- getSourcePos
            E.throwError $ Err pos ("not function: " ++ show f)

eval (Pair e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return $ Pair v1 v2

eval x = return x

evaluate (At _ e@(FunDef _ _ _)) = evaluate e
evaluate e@(FunDef name args body) = do
    result <- runLocally (eval e)
    updateVal name result
    return result
evaluate (At _ (Expr [e])) = evaluate e
evaluate (Expr [e]) = evaluate e
evaluate e = runLocally (eval e)

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
apply e _ = do
    pos <- getSourcePos
    E.throwError $ Err pos ("not function: " ++ show e)

evalWith env val = withEnv env (eval val)


