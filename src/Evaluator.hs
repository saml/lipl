module Evaluator where

import qualified Control.Monad.Reader as M
import qualified Control.Monad.Identity as M
import qualified Control.Monad.Error as M
import qualified Data.Map as Map
import Data.Maybe

import qualified Parser

interpret :: String -> String
interpret input = case Parser.parse input of
    Left err -> show err
    Right val -> "==> " ++ show val
    --Right val -> "==> " ++ show (runEval Map.empty (eval val))

data Val = IntVal Integer
    | FunVal { ident :: String
        , arity :: Int, env :: Env, body :: Parser.Expr }
    | NullVal
    deriving (Show)

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

