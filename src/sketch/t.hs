{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import System.IO
import qualified Data.Map as Map
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Error as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.State as S
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

data Val = Null | Ident String | Int Integer | Expr [Val]
    deriving (Show)

exampleSet = Expr [Ident "=", Ident "foo", Int 42]
example = Ident "foo"

type Env = Map.Map String Val
type ErrMsg = String

newtype Wrap a = Wrap {
    runWrap :: E.ErrorT ErrMsg (S.StateT Env IO) a
} deriving (Functor, Monad, E.MonadError ErrMsg, S.MonadState Env, T.MonadIO)

putVal key val = do
    st <- S.get
    if Map.member key st
        then
            E.throwError ("redefinition: " ++ key)
        else
            S.put $ Map.insert key val st

getVal key = do
    st <- S.get
    case Map.lookup key st of
        Just val -> return val
        otherwise -> E.throwError ("not found: " ++ key)

runEval env val = --I.runIdentity
    (S.runStateT (E.runErrorT $ runWrap val) env)

eval :: Val -> Wrap Val
eval e@(Ident var) = do
    val <- getVal var
    return val
eval e@(Int _) = return e
eval (Expr [Ident name, Ident "=", body]) = do
    val <- eval body
    putVal name val
    return val
eval (Expr [e]) = eval e

main = S.runStateT (E.runErrorT (runWrap repl)) Map.empty

repl :: Wrap ()
repl = do
    line <- T.liftIO $ prompt ">>> "
    case line of
        ":q" -> return ()
        otherwise -> do
            result <- parseAndEval line
            T.liftIO $ putStrLn (show result)
            repl

prompt p = do
    putStr p
    hFlush stdout
    getLine

promptM p = T.liftIO $ prompt p

loopUntil pred askAndGet action = do
    input <- askAndGet
    if pred input
        then
            return ()
        else
            do
                action input
                loopUntil pred askAndGet action

parseAndEval input = case P.parse parseExpr "demo" input of
    Left err -> E.throwError ("parse error: " ++ show err)
    Right val -> eval val

parseInt = do
    val <- P.many1 P.digit
    return $ Int $ read val

parseIdent = do
    val <- P.string "=" <|> P.many1 P.letter
    return $ Ident val

parseExpr = do
    val <- P.sepEndBy parseToken (P.skipMany1 P.space)
    P.eof
    return $ Expr val

parseToken = P.try parseInt <|> P.try parseIdent

