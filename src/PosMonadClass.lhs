=================
PosMonadClass.lhs
=================

.. sectnum::
.. contents::

PosMonadClass provides MonadPos class (interface):
setSourcePos and getSourcePos.
These actions can be used to store current source code position
so that in case of error, the LIPL interpreter can print
location of error.

.. sc:: lhs

> module PosMonadClass where
>
> import qualified Text.ParserCombinators.Parsec as P
> import qualified Text.ParserCombinators.Parsec.Pos as P
> import qualified Control.Monad.Error as E
>
> import Error
>
> class (Monad m) => MonadPos m where
>     setSourcePos :: P.SourcePos -> m ()
>     getSourcePos :: m P.SourcePos

An instance of MonadPos supports setSourcePos and getSourcePos actions.
setSourcePos stores SourcePos.
getSourcePos retrieves stored SourcePos.

This sounds a lot like State monad.
It might be redundant to create this thin wrapper around
State monad. However, one benefit would be an instance of MonadPos
can use something other than State monad in the future.
Then, existing code need not be changed (If existing codes
used get and put action from State monad, those actions should
be replaced with different actions when one refactors).

.. sc:: lhs

> throwErr msg = do
>     pos <- getSourcePos
>     E.throwError $ Err pos msg

throwErr takes a String and throws an Err with current SourcePos.

