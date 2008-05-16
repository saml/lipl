{-# LANGUAGE GeneralizedNewtypeDeriving
    , FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}

module EvalMonad (
    module EvalMonadClass
    , module EvalMonad
) where

import qualified Control.Monad.Error as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Trans as T
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import qualified Control.Monad as M
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import LangData
import Error
import Stack
import TIMonadClass
import EvalMonadClass
import PosMonadClass

{-
newtype Eval a = Eval {
    runEval :: E.ErrorT Err (S.StateT EnvStack I.Identity) a
    } deriving (Functor, Monad
        , E.MonadError Err, S.MonadState EnvStack)

instance MonadEval Eval where
    getEnv = do
        envs <- S.get
        return $ (fst . pop) envs

    getEnvs = S.get

    putEnvs = S.put

    pushEnv env = do
        envs <- S.get
        S.put (push env envs)

    popEnv = do
        envs <- S.get
        if nullEnv == envs
            then return ()
            else S.put $ (snd . pop) envs
-}

newtype Eval a = Eval { runEval :: EvalT I.Identity a }
    deriving (Monad, Functor
        ,  S.MonadState EnvStack
        , MonadEval)

getVal key = do
    envs <- getEnvs
    case (catMaybes $ map (Map.lookup key) envs) of
        (x:_) -> return x
        otherwise -> do
            pos <- getSourcePos
            E.throwError $ Err pos ("not found: " ++ key)

getEnvFor keys = do
    vals <- mapM getVal keys
    let env = Map.fromList $ zip keys vals
    return env

putVal key val = do
    (env:envs) <- getEnvs
    if Map.member key env
        then
            do
                pos <- getSourcePos
                E.throwError
                    $ Err pos ("destructive update: " ++ key)
        else
            putEnvs (Map.insert key val env : envs)

updateVal key val = do
    (env:envs) <- getEnvs
    putEnvs (Map.insert key val env : envs)


clearEnv :: (MonadEval m) => m ()
clearEnv = putEnvs nullEnv



{-
newtype Wrap a = Wrap {
    runWrap :: (E.ErrorT Err (S.StateT EnvStack IO)) a
} deriving (
    Functor, Monad
    , E.MonadError Err, S.MonadState EnvStack, T.MonadIO)

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

-}

newtype EvalT m a = EvalT {
    runEvalT ::  (S.StateT EnvStack m) a
    } deriving(Monad, Functor, R.MonadReader r, W.MonadWriter w
        ,  S.MonadState EnvStack, T.MonadIO)

instance T.MonadTrans EvalT where
    lift m = EvalT ((T.lift m))

instance (Monad m) => MonadEval (EvalT m) where
    getEnv = do
        envs <- S.get
        return $ (fst . pop) envs

    getEnvs = S.get

    putEnvs = S.put

    pushEnv env = do
        envs <- S.get
        S.put (push env envs)

    popEnv = do
        envs <- S.get
        if nullEnv == envs
            then return ()
            else S.put $ (snd . pop) envs
{-
instance (T.MonadIO m) => T.MonadIO (EvalT m) where
    liftIO = T.lift . T.liftIO

instance (R.MonadReader r m) => R.MonadReader r (EvalT m) where
    ask =  T.lift R.ask
    local f m = EvalT (R.local f (runEvalT m))

instance (W.MonadWriter w m) => W.MonadWriter w (EvalT m) where
    tell = T.lift . W.tell
    listen m = EvalT (W.listen (runEvalT m))
    pass m = EvalT (W.pass (runEvalT m))



instance (MonadTI m) => MonadTI (EvalT m) where
    getSubst = T.lift getSubst
    putSubst = T.lift . putSubst
    extendSubst  = T.lift . extendSubst
    newId = T.lift newId
    getN = T.lift getN
    putN = T.lift . putN


-}
instance (E.MonadError e m) => E.MonadError e (EvalT m) where
    throwError = T.lift . E.throwError
    m `catchError` h = EvalT (runEvalT m `E.catchError`
        (\e -> runEvalT (h e)))

instance (MonadPos m) => MonadPos (EvalT m) where
    setSourcePos = T.lift . setSourcePos
    getSourcePos = T.lift getSourcePos

runEvalMonad action =
    I.runIdentity $
        (S.runStateT $  runEvalT $ runEval action)
        nullEnv
