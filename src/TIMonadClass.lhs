================
TIMonadClass.lhs
================

.. sectnum::
.. contents::

TIMonadClass declares class MonadTI.

During type inference of LIPL expressions, a Subst
should be maintained that contains type information of
various type variables.
During evaluation of LIPL expressions, an EnvStack
should be maintained that contains variables
and values bound to them.
While the interpreter is running, various errors should be reported.

A monad can be used to manage a Subst for type inference,
an EnvStack for evaluation, and also handle error reporting.
For modularity, however, different monads are created each of which is
responsible for the Subst, the EnvStack, and error reporting.
Then, a huge monad is constructed by composing those modular monads.

For composition of monads, monad transformers are defined and used,
in a similar fashion that can be found in mtl (Monad Transformer Library.
Comes with GHC): definition of a monad is done over 2 modules:
class module and monad module.
For example, Reader monad in mtl is defined in 2 modules:

- Control.Monad.Reader.Class: provides interface MonadReader
- Control.Monad.Reader: provides Reader monad and ReaderT
  monad transformer.

Similarly, TIMonadClass module and TIMonad module
are responsible for TI monad:

- TIMonadClass: provides interface MonadTI
- TIMonad: provides TI monad and TIT monad transformer.

.. sc:: lhs

> module TIMonadClass where
>
> import Type
>
> class (Monad m) => MonadTI m where
>     getSubst :: m Subst
>     putSubst :: Subst -> m ()
>     extendSubst :: Subst -> m ()
>     getN :: m Int
>     putN :: Int -> m ()
>     newId :: m Id

For a monad to be MonadTI, it has to implement those functions
listed above: getSubst, putSubst, ...

- getSubst should return a Subst in the monad.
- putSubst takes a Subst and puts it to the monad.
- extendSubst takes a Subst and merges it to the current Subst.
  The Subst being merged into shadows current Subst.
  For example, if ``[("a", tNew)]`` is passed to extendSubst
  and current Subst has ``("a", tOld)``, then a is bound to
  tNew::

    current Subst:     [(a, tOld), ...]
    extendSubst        [(a, tNew)]
    ==> current Subst: [(a, tNew), ...]

- getN returns an Int in the monad.
- putN puts an Int to the monad.
- newId returns a new Id (String). It should be unique.

So, MonadTI stores a Subst and an Int.
Users can access the Subst and the Int using getSubst, getN, ...etc.
The Subst can be extended.
And, MonadTI can generate brand new Id.

``class (Monad m) => MonadTI m ...``
means MonadTI class is being declared in the context that says
m is a Monad: Monad is another typeclass just like MonadTI::

    ghci> :i Monad
    class Monad m where
      (>>=) :: m a -> (a -> m b) -> m b
      (>>) :: m a -> m b -> m b
      return :: a -> m a
      fail :: String -> m a
        -- Defined in GHC.Base

``:i`` is GHCi command that prints information of given expression.
So, for a value to be a Monad, it has to support ``>>=``, ``>>``,
return, and fail.

Since MonadTI is declared for all m such that m is a Monad,
to instantiate MonadTI, one should implement ``>>=``, ``>>``, ... also
(if the type trying to instantiate MonadTI isn't already a Monad).

