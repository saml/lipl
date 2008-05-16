{-# LANGUAGE GeneralizedNewtypeDeriving
    , FlexibleInstances #-}

module REPLMonad where

import qualified Control.Monad.Trans as T
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S

import TIMonad
import EvalMonad
import Error
import TCheck
import PosMonad

newtype REPL a = REPL {
    runREPL :: E.ErrorT Err (TIT (EvalT (PosT IO))) a
    } deriving (Monad, Functor
        , MonadTI, MonadEval, MonadPos
        , E.MonadError Err, T.MonadIO)

instance (MonadEval m) => MonadEval (E.ErrorT Err m) where
    getEnv = T.lift getEnv
    getEnvs = T.lift getEnvs
    putEnvs = T.lift . putEnvs
    pushEnv = T.lift . pushEnv
    popEnv = T.lift popEnv

instance (MonadTI m) => MonadTI (E.ErrorT Err m) where
    getSubst = T.lift getSubst
    putSubst = T.lift . putSubst
    extendSubst  = T.lift . extendSubst
    newId = T.lift newId
    getN = T.lift getN
    putN = T.lift . putN

instance (MonadPos m) => MonadPos (E.ErrorT Err m) where
    getSourcePos = T.lift getSourcePos
    setSourcePos = T.lift . setSourcePos

rollBackOnErr action = do
    envs <- getEnvs
    s <- getSubst
    n <- getN
    pos <- getSourcePos
    result <- action `E.catchError` (\e -> do
        putEnvs envs
        putSubst s
        putN n
        setSourcePos pos
        E.throwError e)
    return result


