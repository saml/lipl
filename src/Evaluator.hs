module Evaluator where

import qualified Control.Monad.Reader as M
import qualified Control.Monad.Identity as M
import qualified Control.Monad.Error as M
import qualified Data.Map as Map
import Data.Maybe

import qualified Text.ParserCombinators.Parsec as P
import Parser (parse)
import LangData (Val(..))
import qualified CoreLib as Core

interpret :: String -> String
interpret input = case parse input of
    Left err -> show err
    Right val -> show (eval val)
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

eval :: Val -> Val
eval (Expr []) = Null
eval (Expr [Expr expr]) = eval $ Expr expr
eval (Expr (Ident fname : args)) = funcall fname $ map eval args
eval x = x

funcall :: String -> [Val] -> Val
funcall fname args = case lookup fname Core.primitives of
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

