===========
TIMonad.lhs
===========

.. sectnum::
.. contents::

TIMonad module implements MonadTI class and provides TI monad
and TIT monad transformer.
TI stands for Type Inference.

.. sc:: lhs

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

.. sc:: lhs

> module TIMonad (
>     module TIMonadClass
>     , module TIMonad
> ) where

TIMonad exports TIMonadClass module and TIMonad module (itself).
So, upon importing TIMonad, one can access MonadTI (defined in TIMonadClass)
and all top level functions defined in this module (TIMonad).

.. sc:: lhs

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

More About Monads
=================

.. sc:: lhs

> newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }

Unlike type synonym, newtype defines a brand new type.
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
State type defined in mtl (monad transformer library)
also stores a function::

    newtype State s a = State {
        runState :: (s -> (a, s))
    }


TIMonad, which is very similar to State, is just a custom State monad.
One would just use State monad from mtl library:
http://haskell.org/ghc/docs/latest/html/libraries/mtl/Control-Monad-State.html
However, a custom monad is created for exercise.

.. sc:: lhs

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

.. sc:: lhs

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
- extendSubst takes a Subst (s') and
  modifies s passed from the previous TI action
  and passes out the modified Subst (``s @@ s'``).
- newId returns "tN" where N is Int came from the previous TI action.
  And it passes successor of n to the next TI action.
  The next TI action will get a brand new Int (successor of
  the previous Int).
- getN returns n passed from the previous TI action.
- putN passes n onto the next TI action.

So, TI monad threads (gets values from previous TI action and passes
out values to the next TI action) Subst and Int values, while
returning a value of arbitrary type.
This gives illusion of having global Subst and Int,
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

``>>=``, monadic bind, is similar to function composition ``.``::

    f . g
    ==> apply g. get return value. call it x.
    ==> apply f to x.

    f >>= (\x -> g)
    ==> compute f. get return value.
    ==> feed the return value to (\x -> g).
    ==> compute g in the environment where the return value
        of f is bound to x.

Function composition (f . g) is::

          +---+
    b --> | f | --> c
          +---+

    .

          +---+
    a --> | g | --> b
          +---+

    ==>

          +---------------+
    a --> | g --> b --> f |  --> c
          +---------------+

where f has type (b -> c) and g has type (a -> b).

Monadic bind (f >>= g) is::

          +------------------+
          | f uses input.    |
    a --> | computes output. |
          | puts it inside   |
          | monad.       +---+
          |              | b |
          +--------------+---+

    >>=

          +------------------+
          | g uses input.    |
    b --> | computes output. |
          | puts it inside   |
          | monad.       +---+
          |              | c |
          +--------------+---+

    ==>

          +-----------------------------------------+
          | f uses input.          g uses input.    |
    a --> | computes output.   +-> computes output. |
          | puts it inside     |   puts it inside   |
          | monad.             |   monad.       +---+
          |                b --+                | c |
          +-------------------------------------+---+

Just like concatenative languages (like Joy) uses
juxtaposition (concatenation) for composition,
Haskell's do notation lets users concatenate aligned monadic
actions to form a monadic bind::

    const negate
    ==> composing two functions: negate and const.
        (const is pop in Joy).
        In Haskell, (negate . const).

    do
        a <- action1
        b <- action2
        action3
    ==> binding action1, action2, and action3.

TI monad, which is a custom State monad, binds TI actions
(that are composition/binding of getSubst, putSubst, getN,...etc)
in a way that Subst and Int are threaded to give illusion of
having global Subst and Int.
Other monads can implement return function and ``>>=``
in a way that the monads have some other features.

Another way of binding monadic actions is to use ``>>`` function::

    action1 >> action2

``>>``, unlike ``>>=``, discards return value of action1
(while ``>>=`` passes the return value onto the function
that takes it and computes action2)::

    ghci> :t (>>)
    (>>) :: (Monad m) => m a -> m b -> m b
    ghci> :t (>>=)
    (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

Using ``<-`` in do notation would transforms concatenation
of aligned actions
into ``>>=`` while other concatenations would be transformed
into ``>>``::

    do
        x <- action1
        action2
    ==> action1 >>= (\x -> action2)

    do
        action1
        action2
    ==> action1 >> action2

.. sc:: lhs

> instance Functor TI where
>     fmap f m = TI (\s n -> let
>         (s',n',x) = runTI m s n
>         in
>             (s', n', f x))

::

    ghci> :t fmap
    fmap :: (Functor f) => (a -> b) -> f a -> f b

TI can also be a Functor: f is called on the 3rd value of the tuple
(which is return value). Functor's interface, fmap, can be used
to transform a value inside a monad.

.. sc:: lhs

> newTVar :: (MonadTI m) => m Type
> newTVar = do
>     v <- newId
>     return (TVar v)

newTVar creates a brand new type variable using newId (newId
generates "t0", "t1", ...).

.. sc:: lhs

> unify t1 t2 = do
>     s <- getSubst
>     u <- mgu (apply s t1) (apply s t2)
>     extendSubst u

unify finds mgu of the 2 types. And, extends Subst in TI monad
with the mgu.

Monad Transformers
==================

Having TI monad is great.
To add more features to TI monad, one can rewrite newtype TI,
return function, and ``>>=``.
For example, to make TI monad to have global String,
one can start with::

    newtype TI a = TI {
        runTI :: Subst -> Int -> String -> (Subst, Int, String, a)
        }

This gets tedious. Also, TI is just a State monad.
There are other kinds of monads that not only threads values
among bound monadic actions but also other things: some monads
might not even thread values.

Using monad transformers, one can stack up different monads to build
a new monad.
For example, one can stack up TI monad on top of IO monad
to make IO actions available::

    +---+  a new monad.
    |TI |  TI monad provides getSust, newId, ...etc.
    |---|  (for TI being an instance of MonadTI)
    |IO |  IO monad provides putStrLn, putChar, ...etc.
    +---+

Actually, TIT IO is the new monad (shown above)
that supports IO actions and TI actions.
Above diagram is misleading because there is no TI monad
at the top of the stack. It is because TIT m is also an instance
of MonadTI (that provides getSubst, newId, ...) for all
m such that m is an instance of Monad class (that provides
return, ``>>=``, ...) that TIT IO both supports all IO actions
and actions declared in MonadTI class.

In mtl (monad transformer library), following convention is used:

- Foo.Class module defines MonadFoo class, which is used as
  interface of Foo monad (MonadTI in TIMonadClass module).
- Foo module defines type Foo and makes Foo instance of Monad.
  Also, type FooT (Foo monad transformer) is defined.
  FooT is made instance of many things so that it can be used
  along with other monad transformers.

Similar to mtl, type TIT is defined and made into TI monad transformer.

.. sc:: lhs

> newtype TIT m a = TIT { runTIT :: Subst -> Int -> m (Subst, Int, a) }

TI was a type constructor that took a type a and returned
a type ``TI a``.
TIT is a type constructor that takes a type m and a and returns
a type TIT m a.
So, to construct a TIT type, one would need 2 types
(while one needs only 1 type to construct TI)::

    ghci> :k TI
    * -> *
    ghci> :k TIT
    (* -> *) -> * -> *

``:k`` is a GHCi command that prints kind information of
given expression.
Kind is type of types:

- 1 has type Int
- "hello" has type String
- Int has type (kind) ``*``
- String has kind ``*``
- TI has kind ``* -> *`` (takes a type and returns a type).
- TIT has kind ``(* -> *) -> * -> *``
  (takes a type constructor that takes a type and returns a type,
  and a type. Then, returns a type).

Since TI has kind ``* -> *``, ``TIT TI Int`` would be a valid type.
However, ``TIT String Int`` won't be a valid type because
String has kind ``*``, not ``* -> *``, which TIT expects.

Kind of TIT, ``(* -> *) -> * -> *`` can be written as
``(* -> *) -> (* -> *)``: takes a type constructor with arity 1
and returns a type constructor with arity 1 (with currying
every function has arity 1. But here, arity 1 is used
to refer to types of kind ``* -> *``).
So, one can imagine TIT taking a monad (can be a type
constructor of kind ``* -> *``, just like State String)
and a type (like Int),
and transforming the monad into another monad by adding
MonadTI actions on top of it (TIT m is also an instance of MonadTI)::

    ghci> :m + Control.Monad.State
    ghci> :l TIMonad
    ghci> :k State String
    State String :: * -> *
    ghci> :k TIT (State String) Int
    TIT (State String) Int :: *
    ghci> :k TIT (State String)
    * -> *
    ghci> :k TI
    * -> *

``:m + ModuleName`` makes exported names of ModuleName
available to GHCi (importing/appending ModuleName).

So, ``TIT (State String)`` is a monad that looks like::

    +--------------+ a new monad.
    | TIT          | TIT m provides putSubst, getN, ...etc
    |--------------|     where m is a monad.
    | State String | State String provides put and get
    +--------------+ that puts a String and returns a String
                     respectively.

A ``TIT (State String)`` action can return any type inside.
And it can use all actions TI and State String
provide: putSubst, getN, put, get, ...etc.

One can stack up more monads by using other transformers like:
StateT, ReaderT, WriterT... to build more complicated monad.

.. sc:: lhs

> instance (Monad m) => Monad (TIT m) where
>     return x = TIT (\s n -> return (s, n, x))
>     m >>= k = TIT (\s n -> do
>         (s', n', x) <- runTIT m s n
>         runTIT (k x) s' n')
>     fail str = TIT (\s n -> fail str)

In the context where m is a Monad, TIT m is also a Monad.
Change from TI's definition of return and ``>>=`` is that
return values of the function ``\s n -> ...`` are monads::

    return (s, n, x) instead of (s, n, x)
    runTIT instead of runTI

    ghci> :t runTI
    runTI :: TI a -> Subst -> Int -> (Subst, Int, a)
    ghci> :t runTIT
    runTIT :: TIT m a -> Subst -> Int -> m (Subst, Int, a)

runTI returns (Subst, Int, a) while runTIT returns m (Subst, Int, a):
(Subst, Int, a) inside a monad, m.

.. sc:: lhs

> instance (Monad m) => Functor (TIT m) where
>     fmap f m = TIT (\s n -> do
>         (s', n', x) <- runTIT m s n
>         return (s', n', f x))

Similarly, the return value above is turned to a monad
(a tuple in a monad) instead of
flat a tuple.

.. sc:: lhs

> instance T.MonadTrans TIT where
>     lift m = TIT (\s n -> do
>         a <- m
>         return (s, n, a))

MonadTrans class' lift function turns a monadic action
into a different monadic action::

    ghci> :t lift
    lift :: (Monad m, MonadTrans t) => m a -> t m a

Let there be a monad M that is built with M1T m
to stack up M1 on top of existing monad (stack)::

    this is monad M
    +----+
    | M1T| M1T m supports m1Action
    |----|     where m is any monad.
    | .. | existing monad stack
    +----+

Let there be monad M2 that is inside the existing monad stack
above::

    +----+
    | M2 | M2 supports m2Action
    +----+

In M, one can't call m2Action although M2 might be part of M::

    do           -- inside M
        m2Action -- can't be done unless m2Action is made into
                 -- M action

To turn m2Action into an action that can be used in M, one can use
lift function::

    do                -- inside M
        lift m2Action -- this is now an action lifted onto M

.. sc:: lhs

> instance (Monad m) => MonadTI (TIT m) where
>     getSubst = TIT (\s n -> return (s, n, s))
>     putSubst s = TIT (\_ n -> return (s, n, ()))
>     extendSubst s' = TIT (\s n -> return (s @@ s', n, ()))
>     newId = TIT (\s n -> return (s, n+1, "t" ++ show n))
>     getN = TIT (\s n -> return (s, n, n))
>     putN n = TIT (\s _ -> return (s, n, ()))

By making TIT m an instance of MonadTI,
getSubst, putSubst,...etc can be used inside a monad
that is built up using TIT monad transformer::

    TIT (State String)
    can now use getSubst, putSubst, ...

.. sc:: lhs

> instance (T.MonadIO m) => T.MonadIO (TIT m) where
>     liftIO = T.lift . T.liftIO

liftIO turns an IO action into current monad action::

    ghci> :t liftIO
    liftIO :: (MonadIO m) => IO a -> m a

So, for TIT m, where m is an instance of MonadIO,
all IO actions are turned into m action by using liftIO first.
Then, the m action is lifted to TIT m action using lift function.

.. sc:: lhs

> instance (E.MonadError e m) => E.MonadError e (TIT m) where
>     throwError = T.lift . E.throwError
>     m `catchError` h = TIT (\s n -> runTIT m s n
>         `E.catchError`
>         \e -> runTIT (h e) s n)

TIT m also supports throwError and catchError actions
by being an instance of MonadError e.

::

    ghci> :m + Control.Monad.Error
    ghci> :t throwError
    throwError :: (MonadError e m) => e -> m a

So, throwError takes an error of type e and returns a monad
that reflects occurrence of the error.
For TIT m, throwError is implemented so that the error ridden m a
(returned by call of throwError) is lifted to TIT m.
catchError runs m. In case of error, handler h is called.

.. sc:: lhs

> instance (R.MonadReader r m) => R.MonadReader r (TIT m) where
>     ask = T.lift R.ask
>     local f m = TIT (\s n -> (R.local f (runTIT m s n)))
>
> instance (S.MonadState s m) => S.MonadState s (TIT m) where
>     get = T.lift S.get
>     put = T.lift . S.put
>
> instance (W.MonadWriter w m) => W.MonadWriter w (TIT m) where
>     tell = T.lift . W.tell
>     listen m = TIT (\s n -> do
>         ((s', n', x), w) <- W.listen (runTIT m s n)
>         return (s', n', (x, w)))
>     pass m = TIT (\s n -> W.pass (do
>         (s', n', (x, f)) <- runTIT m s n
>         return ((s', n', x), f)))
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

Similarly, different classes are implemented for TIT m so that
various actions can be used inside TIT m without explicit lift.

MonadEval and MonadPos are to be defined in EvalMonadClass
and PosMonadClass modules respectively.


