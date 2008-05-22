{-# LANGUAGE GeneralizedNewtypeDeriving
    , FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}

module PosMonad (
    module PosMonadClass
    , module PosMonad
) where

import qualified Control.Monad.Error as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Trans as T
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import qualified Control.Monad as M
import qualified Text.ParserCombinators.Parsec as P

import PosMonadClass
import TIMonad
import EvalMonad

newtype Pos a = Pos {
    runPos :: PosT I.Identity a
    } deriving (Monad, Functor
        , S.MonadState P.SourcePos
        , MonadPos)

newtype PosT m a = PosT {
    runPosT :: (S.StateT P.SourcePos m) a
    } deriving (Monad, Functor, S.MonadState P.SourcePos
        , R.MonadReader r, W.MonadWriter w, E.MonadError e
        , T.MonadIO)

instance T.MonadTrans PosT where
    lift m = PosT (T.lift m)

{-
instance (E.MonadError e m) => E.MonadError e (PosT m) where
    throwError = T.lift . E.throwError
    m `catchError` h = PosT (runPosT m `E.catchError`
        (\e -> runPosT (h e)))
-}

instance (Monad m) => MonadPos (PosT m) where
    setSourcePos pos = S.put pos
    getSourcePos = S.get

instance (MonadTI m) => MonadTI (PosT m) where
    getSubst = T.lift getSubst
    putSubst = T.lift . putSubst
    extendSubst  = T.lift . extendSubst
    newId = T.lift newId
    getN = T.lift getN
    putN = T.lift . putN

instance (MonadEval m) => MonadEval (PosT m) where
    getEnv = T.lift getEnv
    getEnvs = T.lift getEnvs
    putEnvs = T.lift . putEnvs
    pushEnv = T.lift . pushEnv
    popEnv = T.lift popEnv

