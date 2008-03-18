module Evaluator where

--import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Data.Map as Map
import Data.Maybe

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
    return val
eval e@(Int _) = return e
eval e@(Float _) = return e
eval e@(Bool _) = return e
eval e@(Char _) = return e
eval e@(Str _) = return e
eval (List xs) = do          -- eager evaluation
    elems <- mapM eval xs
    return $ List elems
eval (Expr []) = return Null
eval (Expr [e]) = eval e -- unwrap outer parens
eval (Expr [Ident "=", Ident name, body]) = do
    putVal name body
    env <- getEnv
    return $ Fun env name [] body
eval (Expr [Ident "if", pred, ifCase, elseCase]) = do
    ifOrElse <- eval pred
    case ifOrElse of
        Bool False -> eval elseCase
        otherwise -> eval ifCase
eval (Expr (Ident fname : args)) = do
    params <- mapM eval args -- eager evaluation
    funcall fname params
eval x = return x
--eval x = E.throwError $ BadExprErr "Bad Expr" x

