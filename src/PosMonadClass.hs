module PosMonadClass where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Pos as P

class (Monad m) => MonadPos m where
    setSourcePos :: P.SourcePos -> m ()
    getSourcePos :: m P.SourcePos

initialPos = P.initialPos ""
