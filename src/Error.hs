module Error where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ ( (<>), (<+>), ($$), ($+$) )
import qualified Control.Monad.Error as E

import LangData
import PosMonadClass (initialPos)

type ErrMsg = String
data Err = Err P.SourcePos ErrMsg

ppErr (Err pos msg) = PP.fsep [PP.text (show pos), PP.text msg]

instance Show Err where
    show e = PP.render $ ppErr e

instance E.Error Err where
    noMsg = Err initialPos "Error"
    strMsg = Err initialPos

