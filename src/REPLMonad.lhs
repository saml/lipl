=============
REPLMonad.lhs
=============

.. sectnum::
.. contents::

REPLMonad defines REPL monad that supports
useful actions: getSubst, putEnv, ...
to build the LIPL interpreter (including read eval print loop).

.. sc:: lhs

> {-# LANGUAGE GeneralizedNewtypeDeriving
>     , FlexibleInstances #-}
>
> module REPLMonad where
>
> import qualified Control.Monad.Trans as T
> import qualified Control.Monad.Error as E
> import qualified Control.Monad.State as S
> import qualified Control.Monad.Writer as W
> import qualified Control.Monad.Reader as R
>
> import TIMonad
> import EvalMonad
> import Error
> import TCheck
> import PosMonad
> import LangData (emptyEnvStack, initialPos)
>
> newtype REPL a = REPL {
>     runREPL :: E.ErrorT Err (TIT (EvalT (PosT IO))) a
>     } deriving (Monad, Functor, MonadEval, MonadTI, MonadPos
>         , E.MonadError Err, T.MonadIO)

REPL looks like this::

    +--------+
    | ErrorT | provides MonadError: throwError and catchError
    |--------|
    | TIT    | provides MonadTI: getSubst, newId,...
    |--------+
    | EvalT  | provides MonadEval: getEnv, putEnvs, ...
    |--------|
    | PosT   | provides MonadPos: getSourcePos and setSourcePos
    |--------|
    | IO     | provides IO actions: putStrLn, getLine, ...
    +--------+

So, in REPL, any of those actions can be used.

ErrorT is at the top of the monad stack because any monad
transformer on top of ErrorT layer might loose their values.
For example::

    ghci> :m + Control.Monad.Error
    ghci> :m + Control.Monad.State
    ghci> :t runState (runErrorT (return 0)) ""
    ... :: (Error e, Num t)
           => (Either e t, [Char])
                 |           +---- state value is intact
                 +---------------- this can be Left e.

    ghci> :t runErrorT (runStateT (return 0) "")
    ... :: (Error e, Monad m, Num t)
           => m (Either e (t, [Char]))
                          +------------ this whole thing might be Left e
                                        in which case the state value,
                                        of type [Char], is lost.

The first case, ``runState (runErrorT ...)``
is when ErrorT is above State monad.
The return value of it does have state value intact (``[Char]``).
Only the return value (of type t, an instance of Num) might
be lost (it could be Left e, not Right t).
The second case can return ``Right (t, [Char])`` with state value
intact. But it could also return ``Left e``, losing ``[Char]`` value.

.. sc:: lhs

> instance MonadTI (E.ErrorT Err (TIT (EvalT (PosT IO)))) where
>     getSubst = T.lift getSubst
>     putSubst = T.lift . putSubst
>     extendSubst = T.lift . extendSubst
>     getN = T.lift getN
>     putN = T.lift . putN
>     newId = T.lift newId
>
> instance MonadPos (E.ErrorT Err (TIT (EvalT (PosT IO)))) where
>     setSourcePos = T.lift . setSourcePos
>     getSourcePos = T.lift getSourcePos
>
> instance MonadEval (E.ErrorT Err (TIT (EvalT (PosT IO)))) where
>     getEnv  = T.lift getEnv
>     getEnvs = T.lift getEnvs
>     putEnvs = T.lift . putEnvs
>     pushEnv = T.lift . pushEnv
>     popEnv  = T.lift popEnv

Since ErrorT is at the top, the monad stack should be made
instance of MonadTI, MonadPos, and MonadEval for magic lifting.
Otherwise, one would have to write::

    lift (lift (lift getSourcePos))

to use MonadPos actions in REPL monad, for example.

.. sc:: lhs

> rollBackOnErr action = do
>     envs <- getEnvs
>     s <- getSubst
>     n <- getN
>     pos <- getSourcePos
>     result <- action `E.catchError` (\e -> do
>         putEnvs envs
>         putSubst s
>         putN n
>         setSourcePos pos
>         E.throwError e)
>     return result

rollBackOnErr caches current state of the interpreter:
Envs, Subst, Int, SourcePos, ...
and executes the given action.
In case of error, cached state is restored and the error
re-thrown.

.. sc:: lhs

> run action = S.runStateT (runPosT (S.runStateT (runEvalT
>     (runTIT (E.runErrorT (runREPL action)) initialSubst 0))
>     emptyEnvStack)) initialPos

run takes a REPL action and executes it, unwrapping all monad transformers,
leaving IO monad, which is at the core of REPL monad.

