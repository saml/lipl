{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EvalMonad where

import qualified Control.Monad.Fix as F
import qualified Control.Monad.Error as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Trans as T
import qualified Control.Monad.State as S
import qualified Control.Monad as M
import qualified Control.Applicative as A
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import LangData
import Error
import Stack

getEnv :: Wrap Env
getEnv = do
    st <- S.get
    return $ (fst . pop) st

getEnvFor :: [Key] -> Wrap Env
getEnvFor keys = do
    vals <- mapM getVal keys
    let env = Map.fromList $ zip keys vals
    return env
    --env <- getEnv
    --let keysEnv = Map.fromList $ map (\k -> (k, Null)) keys
    --return $ env `Map.intersection` keysEnv

newtype Wrap a = Wrap {
    runWrap :: E.ErrorT Err (S.StateT EnvStack IO) a
} deriving (
    Functor, Monad, F.MonadFix
    , E.MonadError Err, S.MonadState EnvStack, T.MonadIO)

instance A.Applicative Wrap where
    pure = return
    (<*>) = M.ap

putVal key val = do
    (st:xs) <- S.get
    if Map.member key st
        then
            E.throwError $ EnvUpdateErr key
        else
            S.put (Map.insert key val st:xs)

updateVal key val = do
    (env:xs) <- S.get
    S.put (Map.insert key val env : xs)

getVal key = do
    envs <- S.get
    case (catMaybes $ map (Map.lookup key) envs) of
        (x:_) -> return x
        otherwise -> E.throwError $ UnboundIdentErr "not found" key

{-
getVal key = do
    (env:xs) <- S.get
    case Map.lookup key env of
        Just val -> return val
        otherwise -> E.throwError $ UnboundIdentErr "not found" key
-}

pushEnv :: Env -> Wrap ()
pushEnv env = do
    st <- S.get
    S.put (push env st)

popEnv :: Wrap ()
popEnv = do
    st <- S.get
    if nullEnv == st
        then return ()
        else S.put $ (snd . pop) st

clearEnv :: Wrap ()
clearEnv = do
    S.put nullEnv

extendPushEnv :: Env -> Wrap ()
extendPushEnv env = do
    envs <- S.get
    if isEmpty envs
        then
            S.put (push env envs)
        else
            do
                let currEnv = head envs
                let newEnvs = push (env `Map.union` currEnv) envs
                S.put newEnvs


