{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
    , FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fallow-undecidable-instances #-}

module TIMonad (
    module TIMonadClass
    , module TIMonad
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Fix as F

import EvalMonadClass
import TIMonadClass
import Type
import PosMonadClass

newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }

instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of
        (s', n', x) -> let TI gx = g x in gx s' n')

instance Functor TI where
    fmap f m = TI (\s n -> let
        (s',n',x) = runTI m s n
        in
            (s', n', f x))

instance MonadTI TI where
    getSubst = TI (\s n -> (s, n, s))
    putSubst s = TI (\_ n -> (s, n, ()))
    extendSubst s' = TI (\s n -> (s @@ s', n, ()))
    newId = TI (\s n -> (s, n+1, "t" ++ show n))
    getN = TI (\s n -> (s, n, n))
    putN n = TI (\s _ -> (s, n, ()))

newTVar :: (MonadTI m) => m Type
newTVar = do
    v <- newId
    return (TVar v)

unify t1 t2 = do
    s <- getSubst
    u <- mgu (apply s t1) (apply s t2)
    extendSubst u

newtype TIT m a = TIT { runTIT :: Subst -> Int -> m (Subst, Int, a) }

instance (Monad m) => Monad (TIT m) where
    return x = TIT (\s n -> return (s, n, x))
    m >>= k = TIT (\s n -> do
        (s', n', x) <- runTIT m s n
        runTIT (k x) s' n')
    fail str = TIT (\s n -> fail str)

instance (Monad m) => Functor (TIT m) where
    fmap f m = TIT (\s n -> do
        (s', n', x) <- runTIT m s n
        return (s', n', f x))

instance T.MonadTrans TIT where
    lift m = TIT (\s n -> do
        a <- m
        return (s, n, a))

instance (Monad m) => MonadTI (TIT m) where
    getSubst = TIT (\s n -> return (s, n, s))
    putSubst s = TIT (\_ n -> return (s, n, ()))
    extendSubst s' = TIT (\s n -> return (s @@ s', n, ()))
    newId = TIT (\s n -> return (s, n+1, "t" ++ show n))
    getN = TIT (\s n -> return (s, n, n))
    putN n = TIT (\s _ -> return (s, n, ()))

instance (T.MonadIO m) => T.MonadIO (TIT m) where
    liftIO = T.lift . T.liftIO

instance (E.MonadError e m) => E.MonadError e (TIT m) where
    throwError = T.lift . E.throwError
    m `catchError` h = TIT (\s n -> runTIT m s n
        `E.catchError`
        \e -> runTIT (h e) s n)

instance (R.MonadReader r m) => R.MonadReader r (TIT m) where
    ask = T.lift R.ask
    local f m = TIT (\s n -> (R.local f (runTIT m s n)))

instance (S.MonadState s m) => S.MonadState s (TIT m) where
    get = T.lift S.get
    put = T.lift . S.put

instance (MonadEval m) => MonadEval (TIT m) where
    getEnv = T.lift getEnv
    getEnvs = T.lift getEnvs
    putEnvs = T.lift . putEnvs
    pushEnv = T.lift . pushEnv
    popEnv = T.lift popEnv

instance (MonadPos m) => MonadPos (TIT m) where
    setSourcePos = T.lift . setSourcePos
    getSourcePos = T.lift getSourcePos


fromIdType = map (\(k,v) -> (k, mkPolyType v))

toSubst :: [(Id, Type)] -> Subst
toSubst = Map.fromList . fromIdType

