=============
EvalMonad.lhs
=============

.. sectnum::
.. contents::

EvalMonad creates a monad, Eval, that implements MonadEval class.
Also, EvalT, a transformer of Eval, is defined.

.. sc:: lhs

> {-# LANGUAGE GeneralizedNewtypeDeriving
>     , FlexibleInstances, MultiParamTypeClasses #-}
> {-# OPTIONS_GHC -fallow-undecidable-instances #-}
>
> module EvalMonad (
>     module EvalMonadClass
>     , module EvalMonad
> ) where
>
> import qualified Control.Monad.Error as E
> import qualified Control.Monad.Identity as I
> import qualified Control.Monad.Trans as T
> import qualified Control.Monad.State as S
> import qualified Control.Monad.Reader as R
> import qualified Control.Monad.Writer as W
> import qualified Control.Monad as M
> import qualified Data.Map as Map
> import Data.Maybe (catMaybes)
>
> import LangData
> import Error
> import Stack
> import TIMonadClass
> import EvalMonadClass
> import PosMonadClass
>
> newtype Eval a = Eval { runEval :: EvalT I.Identity a }
>     deriving (Monad, Functor
>         ,  S.MonadState EnvStack
>         , MonadEval)

Eval monad is defined using EvalT transformer::

    +----------+
    | Eval     | all MonadEval actions are supported
    |----------| (getEnv, putEnvs,...)
    | Identity | Identity monad is usually used as base monad.
    +----------+

Eval monad also derives (automatically implements)
Monad, Functor, MonadState EnvStack, and MonadEval classes.

.. sc:: lhs

> getVal key = do
>     envs <- getEnvs
>     case (catMaybes $ map (Map.lookup key) envs) of
>         (x:_) -> return x
>         otherwise -> do
>             pos <- getSourcePos
>             E.throwError $ Err pos ("not found: " ++ key)

getVal returns Val that is bound to key.
Since Eval monad has a global EnvStack,
the EnvStack is searched from top to bottom for a Val bound to key.

::

    ghci> :m + Data.Maybe
    ghci> :t catMaybes
    catMaybes :: [Maybe a] -> [a]
    ghci> catMaybes [Just 1, Nothing, Just 2]
    [1,2]
    ghci> catMaybes []
    []
    ghci> catMaybes [Nothing]
    []

.. sc:: lhs

> getEnvFor keys = do
>     vals <- mapM getVal keys
>     let env = Map.fromList $ zip keys vals
>     return env

getEnvFor returns an Env that contains all Vals bound to
given list of keys::

    getEnvFor ["a", "b"] -- in a monad that has environment
                         -- {a = 1, b = "hi", c = 3}
    ==> {a = 1, b = "hi"}

.. sc:: lhs

> putVal key val = do
>     env <- getEnv
>     if Map.member key env
>         then
>             do
>                 pos <- getSourcePos
>                 E.throwError
>                     $ Err pos ("destructive update: " ++ key)
>         else
>             pushEnv (Map.insert key val env)
>
> updateVal key val = do
>     (env:envs) <- getEnvs
>     putEnvs (Map.insert key val env : envs)

putVal takes a key and a Val and updates the top Env
so that the Val is bound to the key.
When key is already bound, it throws an exception
(a key can be bound only once).
updateVal, however, allows destructive update (a key can be bound
multiple times).

.. sc:: lhs

> clearEnvs :: (MonadEval m) => m ()
> clearEnvs = putEnvs emptyEnvStack

clearEnvs clears the global EnvStack.

.. sc:: lhs

> newtype EvalT m a = EvalT {
>     runEvalT ::  (S.StateT EnvStack m) a
>     } deriving(Monad, Functor
>         , R.MonadReader r, W.MonadWriter w, E.MonadError e
>         ,  S.MonadState EnvStack, T.MonadIO)

EvalT, used to build Eval monad, is defined to be a monad
built with StateT EnvStack::

    +-----------------+
    | StateT EnvStack | MonadState actions supported: put, get, ...
    |-----------------|
    | m               |
    +-----------------+

EvalT alone expects a monad m to transform it to a new monad
by adding State monad on top of it.
So, EvalT Identity (which is how Eval monad is built)
would be a monad that is very similar to
just plain State EnvStack monad.

By deriving many classes (Monad, Functor, MonadReader,...),
EvalT can be composed with other monad transformers (such as
ReaderT, WriterT,...) to build a complicated monad
while still allowing actions to be used without explicit lift::

    newtype EvalT m a = EvalT (StateT EnvStack m) a

Given above declaration of EvalT::

    do
        put emptyEnvStack

one can't run above inside EvalT m monad when EvalT
did not derive MonadState EnvStack.
One should use explicit lift::

    do
        lift (put emptyEnvStack)

However, since EvalT derives MonadState EnvStack,
one can use ``put emptyEnvStack`` without explicit lift
(it gets lifted magically).

In the same way, MonadReader, MonadWriter, ... actions
can be used without explicit lift in EvalT m monad since
EvalT derives them.

.. sc:: lhs

> instance T.MonadTrans EvalT where
>     lift m = EvalT (T.lift m)
>     -- lift = EvalT . T.lift

To function as a transformer, EvalT should implement lift function
that turns a monadic action m to EvalT t action
by first lifting m to t m and wrapping t m in EvalT.

.. sc:: lhs

> instance (Monad m) => MonadEval (EvalT m) where
>     getEnv = do
>         envs <- S.get
>         return $ (fst . pop) envs
>
>     getEnvs = S.get
>
>     putEnvs = S.put
>
>     pushEnv env = do
>         envs <- S.get
>         S.put (push env envs)
>
>     popEnv = do
>         envs <- S.get
>         if emptyEnvStack == envs
>             then return ()
>             else S.put $ (snd . pop) envs

MonadEval actions are implemented using MonadState actions (pop and get).
popEnv does not do anything when the EnvStack is already empty
(nothing to pop).

.. sc:: lhs

> instance (MonadTI m) => MonadTI (EvalT m) where
>     getSubst = T.lift getSubst
>     putSubst = T.lift . putSubst
>     extendSubst  = T.lift . extendSubst
>     newId = T.lift newId
>     getN = T.lift getN
>     putN = T.lift . putN

By making EvalT m an instance of MonadTI for all m that is an instance
of MonadTI, EvalT m can use MonadTI actions (getSubst, putSubst,...).

.. sc:: lhs

> {- instance (E.MonadError e m) => E.MonadError e (EvalT m) where
>     throwError = T.lift . E.throwError
>     m `catchError` h = EvalT (runEvalT m `E.catchError`
>         (\e -> runEvalT (h e)))
> -}
> instance (MonadPos m) => MonadPos (EvalT m) where
>     setSourcePos = T.lift . setSourcePos
>     getSourcePos = T.lift getSourcePos

EvalT m can use MonadPos actions, too.
