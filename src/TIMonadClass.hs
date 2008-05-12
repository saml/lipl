module TIMonadClass where

import Type

class (Monad m) => MonadTI m where
    getSubst :: m Subst
    putSubst :: Subst -> m ()
    extendSubst :: Subst -> m ()
    newId :: m Id
