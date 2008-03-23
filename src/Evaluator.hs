module Evaluator where

--import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.Trans as T
import qualified Control.Exception as Ex
import qualified Data.Map as Map

import Data.Maybe
import qualified Data.List as List
import Data.List ((\\))

import qualified Text.ParserCombinators.Parsec as P
import Parser
import LangData
import CoreLib

import Debug.Trace (trace)

--runEval :: EnvStack -> Wrap Val -> (Either Err Val, EnvStack)
{-
runEval env val = I.runIdentity
    (S.runStateT (E.runErrorT $ runWrap val) env)
-}
{-
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
-}
--apply :: ValM -> ValM -> ValM

--eval :: Val -> Wrap Val
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
    return $ Prim name (arityOf name) []

eval (Lambda args body) = do
    let funEmptyEnv = Fun emptyEnv args body
    let keys = freeVars funEmptyEnv
    if null keys
        then
            return funEmptyEnv
        else
            do
                env <- getEnvFor keys
                return $ Fun env args body


eval (FunDef name args body) = do
    let funEmptyEnv = Fun emptyEnv args body
    let keys = freeVars funEmptyEnv
    if null keys
        then
            do
                putVal name funEmptyEnv
                return funEmptyEnv
        else
            do
                env <- getEnvFor keys
                let fun = Fun env args body
                putVal name fun
                return fun

eval (If pred ifCase elseCase) = do
    ifOrElse <- eval pred
    case ifOrElse of
        Bool False -> eval elseCase
        otherwise -> eval ifCase

eval (Let env body) = withEnv env (eval body)
{-
    extendPushEnv env
    val <- eval body
    popEnv
    return val
-}

--eval (IOFun name arg) = T.liftIO $ putStrLn (show arg)

eval (Fun env args body) | null args = withEnv env (eval body)

eval (Expr []) = return Null
eval (Expr [e]) = eval e -- unwrap outer parens
eval (Expr [Ident "=", Ident name, body]) = do
    putVal name body
    let keys = freeVars body
    env <- getEnvFor keys
    return $ Fun env [] body

eval (Expr (f:e)) = do -- both f and e are Val because Expr [e] case
    let firstArg = head e
    let restArgs = tail e
    function <- eval f
    case function of
        fun@(Fun env _ _) ->
            evalFun env fun firstArg restArgs
        fun@(Prim name remaining args) ->
            evalPrim name firstArg restArgs args remaining
        otherwise -> E.throwError $ NotFunErr "" (show f)
{-
-- | primitive function application
eval (Expr (Ident fname : args)) = do
    params <- mapM eval args -- eager evaluation
    funcall fname params
-}
eval x = return x
--eval x = E.throwError $ BadExprErr "Bad Expr" x

{-
evalFun env fun firstArg restArgs = do
    extendPushEnv env
    arg1 <- eval firstArg
    partial <- apply fun arg1
    val <- eval $ Expr (partial : restArgs)
    popEnv
    return val
-}

evalFun env fun firstArg restArgs =
    withEnv env (do
        arg1 <- eval firstArg
        partial <- apply fun arg1
        eval $ Expr (partial : restArgs))

evalPrim fname firstArg restArgs argsHave remaining = do
    arg1 <- eval firstArg
    let args = argsHave ++ [arg1]
    if remaining == 1
        then
            funcall fname args
        else
            eval $ Expr (
                (Prim fname (remaining-1) args) : restArgs)

--withEnv :: Env -> Wrap a -> Wrap a
withEnv env action = do
    (do
        extendPushEnv env
        val <- action
        popEnv
        return val)
    `E.catchError`
    (\e -> do
        popEnv
        E.throwError e)
--    (extendPushEnv env >> action) `Ex.finally` popEnv


apply :: Val -> Val -> Wrap Val
apply (Fun env (arg:rst) body) e = do
    let env' = Map.insert arg e env
    if null rst
        then
            withEnv env' (eval body)
            {-
            do
                extendPushEnv env'
                val <- eval body
                popEnv
                return val
            -}
        else
            return $ Fun env' rst body


freeVars (Ident a) = [a]
freeVars (Let env body) = freeVars body \\ Map.keys env
freeVars (Lambda params body) = freeVars body \\ params
freeVars (Fun env params body) = freeVars body \\ params
freeVars (Prim _ _ []) = []
freeVars (Prim _ _ (x:xs)) = freeVars x `List.union` freeVars (Expr xs)
freeVars (Expr (x:xs)) = freeVars x `List.union` freeVars (Expr xs)
freeVars (Expr []) = []
freeVars x = []

{-
freeVarsList (Ident a:xs) = a : freeVarsList xs
freeVarsList (_:xs) = freeVarsList xs
freeVarsList [] = []
--freeVarsList = filter (null . freeVars)
-}

