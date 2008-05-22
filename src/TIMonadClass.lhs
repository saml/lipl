================
TIMonadClass.lhs
================

TIMonadClass declares interface for MonadTI.

.. sc:: haskell

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
  tNew.
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

