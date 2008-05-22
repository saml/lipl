===========
TIMonad.lhs
===========

TIMonad module implements MonadTI class.
Type inference will use (run in) TIMonad.

.. sc:: haskell

> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
>     , FlexibleInstances, GeneralizedNewtypeDeriving #-}
> {-# OPTIONS_GHC -fallow-undecidable-instances #-}

Above pragmas and option are needed.
Their meanings can be found in:
http://www.haskell.org/ghc/docs/latest/html/users_guide/flag-reference.html

LANGUAGE pragma enables language extensions that are not part of haskell98
standard.
GHC suggests extensions to enable when it can't compile a module.
So, one can include proper pragmas by trial and error.

.. sc:: haskell

> module TIMonad (
>     module TIMonadClass
>     , module TIMonad
> ) where

TIMonad exports what TIMonadClass module and TIMonad module (itself).
So, upon importing TIMonad, one can access MonadTI (defined in TIMonadClass)
and all top level functions defined in this module (TIMonad).

.. sc:: haskell

> import qualified Data.List as List
> import qualified Data.Map as Map
> import qualified Control.Monad.Error as E
> import qualified Control.Monad.State as S
> import qualified Control.Monad.Reader as R
> import qualified Control.Monad.Writer as W
> import qualified Control.Monad.Trans as T
> import qualified Control.Monad.Fix as F
>
> import EvalMonadClass
> import TIMonadClass
> import Type
> import PosMonadClass
>
> newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }

Unlike type synonym, newtype declares a brand new type.
For example, after defining type synonym ``type MyInt = Int``,
MyInt can be used where Int can be used.
However, after ``newtype MyInt = MyInt Int``, MyInt is a different type.
So, newtype is similar to data declaration. But, only 1 data
constructor containing a single field is allowed.

TI is a type constructor that takes a type and returns another type.
TI is also a data constructor that takes
a function of type ``Subst -> Int -> (Subst, Int, a)``
(takes a Subst and an Int and returns a tuple of a Subst, an Int
and a value of type a)
and returns a value of type TI a.
The type variable a in ``TI a`` will be determined by the
function passed in to TI data constructor.
For example::

    TI (\s i -> (s, i, 1)) :: TI Int
    TI (\s i -> (s, i, "hello")) :: TI String

So, a value of type TI stores a function.
State type defined in mtl library also stores a function::

    newtype State s a = State {
        runState :: (s -> (a, s))
    }


TIMonad, which is very similar to State, is just a custom State monad.
One would just use State monad from mtl library:
http://haskell.org/ghc/docs/latest/html/libraries/mtl/Control-Monad-State.html
However, a custom monad is created for exercise.

.. sc:: haskell

> instance Monad TI where
>     return x = TI (\s n -> (s, n, x))
>     f >>= g = TI (\s n -> let (s', n', x) = runTI f s n
>         in
>             runTI (g x) s' n')

This mimics how State monad is implemented.
For TI to be a Monad, it implements 2 functions from Monad class
(other functions from Monad class have default implementation
that works properly given these 2 functions):
return and ``>>=``.

return function returns a value of type TI whose function
(TI stores a function)
puts the argument, x, on the 3rd place of the tuple.

``>>=`` returns a value of type TI whose function
takes s and n (Subst and Int) and calls runTI on
f (of type TI a), s, and n.
runTI has type ``TI a -> Subst -> Int -> (Subst, Int, a)``.
So, runTI f s n returns a value of type (Subst, Int, a).
(s', n', x) is the return value of runTI.
Then, it calls runTI again on g x, s', and n'.

::

    ghci> :t (>>=)
    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

``:t`` is GHCi command that prints type of the given expression.
So, f in ``f >>= g`` has type TI a.
And, g has type ``a -> TI b``.
So, ``runTI (g x) s' n'`` returns (Subst, Int, b) because (g x)
has type TI b.

All this can be summarized as::

    TI (\s n -> (a, b, c))
         | |     |  |  +--- value to be returned in TI monad.
         | |     |  +------ Int value passed to next TI action.
         | |     +--------- Subst value passed to next TI action.
         | +--------------- Int value from previous TI action.
         +----------------- Subst value from previous TI action.


Start GHCi and try these::

    ghci> :l TIMonad
    ghci> runTI (return 1) nullSubst 0
    (fromList [],0,1)
    ghci> runTI (return "hello") nullSubst 0
    (fromList [],0,"hello")

Just as return function is implemented, return whatever
puts whatever on the 3rd position of the tuple.

::

    ghci> let action = return 1 >>= (\x -> return (x + 1)) :: TI Int
    ghci> runTI action nullSubst 100
    (fromList [],100,2)

.. sc:: haskell

> instance Functor TI where
>     fmap f m = TI (\s n -> let
>         (s',n',x) = runTI m s n
>         in
>             (s', n', f x))

::

    ghci> :t fmap
    fmap :: (Functor f) => (a -> b) -> f a -> f b

TI can also be a Functor: f is called on the 3rd value of the tuple
(which is return value).

.. sc:: haskell

> instance MonadTI TI where
>     getSubst = TI (\s n -> (s, n, s))
>     putSubst s = TI (\_ n -> (s, n, ()))
>     extendSubst s' = TI (\s n -> (s @@ s', n, ()))
>     newId = TI (\s n -> (s, succ n, "t" ++ show n))
>     getN = TI (\s n -> (s, n, n))
>     putN n = TI (\s _ -> (s, n, ()))

TI being a Monad, it can now be an instance of MonadTI.

- getSubst just returns current s (Subst) by putting it at the 3rd place
    of the tuple.
- putSubst ignores current Subst being passed to (_). And passes s
    on to the next TI action (by putting s on 1st place of the tuple).
- extendSubst modifies s passed from the previous TI action
    and passes out the modified Subst.
- newId returns "tN" where N is Int came from the previous TI action.
    And it passes successor of n to the next TI action.
- getN returns n passed from the previous TI action.
- putN passes n onto the next TI action.

So, TI monad threads (gets values from previous TI action and passes
out values to the next TI action) Subst and Int values, while
returning arbitrary value.
This gives illusion of having a global Subst and Int,
in addition to a value of arbitrary type, inside TI monad.

It might be tedious to combine actions like::

    action1 >>= (\x -> action2) >>= (\y -> action3)

So, one could use do notation instead::

    do
        x <- action1
        y <- action2
        action3

    or

    do { x <- action1; y <- action2;
        action3 }

.. sc:: haskell

> newTVar :: (MonadTI m) => m Type
> newTVar = do
>     v <- newId
>     return (TVar v)
>
> unify t1 t2 = do
>     s <- getSubst
>     u <- mgu (apply s t1) (apply s t2)
>     extendSubst u
>
> newtype TIT m a = TIT { runTIT :: Subst -> Int -> m (Subst, Int, a) }
>
> instance (Monad m) => Monad (TIT m) where
>     return x = TIT (\s n -> return (s, n, x))
>     m >>= k = TIT (\s n -> do
>         (s', n', x) <- runTIT m s n
>         runTIT (k x) s' n')
>     fail str = TIT (\s n -> fail str)
>
> instance (Monad m) => Functor (TIT m) where
>     fmap f m = TIT (\s n -> do
>         (s', n', x) <- runTIT m s n
>         return (s', n', f x))
>
> instance T.MonadTrans TIT where
>     lift m = TIT (\s n -> do
>         a <- m
>         return (s, n, a))
>
> instance (Monad m) => MonadTI (TIT m) where
>     getSubst = TIT (\s n -> return (s, n, s))
>     putSubst s = TIT (\_ n -> return (s, n, ()))
>     extendSubst s' = TIT (\s n -> return (s @@ s', n, ()))
>     newId = TIT (\s n -> return (s, n+1, "t" ++ show n))
>     getN = TIT (\s n -> return (s, n, n))
>     putN n = TIT (\s _ -> return (s, n, ()))
>
> instance (T.MonadIO m) => T.MonadIO (TIT m) where
>     liftIO = T.lift . T.liftIO
>
> instance (E.MonadError e m) => E.MonadError e (TIT m) where
>     throwError = T.lift . E.throwError
>     m `catchError` h = TIT (\s n -> runTIT m s n
>         `E.catchError`
>         \e -> runTIT (h e) s n)
>
> instance (R.MonadReader r m) => R.MonadReader r (TIT m) where
>     ask = T.lift R.ask
>     local f m = TIT (\s n -> (R.local f (runTIT m s n)))
>
> instance (S.MonadState s m) => S.MonadState s (TIT m) where
>     get = T.lift S.get
>     put = T.lift . S.put
>
> instance (MonadEval m) => MonadEval (TIT m) where
>     getEnv = T.lift getEnv
>     getEnvs = T.lift getEnvs
>     putEnvs = T.lift . putEnvs
>     pushEnv = T.lift . pushEnv
>     popEnv = T.lift popEnv
>
> instance (MonadPos m) => MonadPos (TIT m) where
>     setSourcePos = T.lift . setSourcePos
>     getSourcePos = T.lift getSourcePos
>
>
> fromIdType = map (\(k,v) -> (k, mkPolyType v))
>
> toSubst :: [(Id, Type)] -> Subst
> toSubst = Map.fromList . fromIdType
>
