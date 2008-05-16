module Error where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Pos as P
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ ( (<>), (<+>), ($$), ($+$) )
import qualified Control.Monad.Error as E

import LangData

type ErrMsg = String
data Err = Err P.SourcePos ErrMsg

ppErr (Err pos msg) = PP.fsep [PP.text (show pos), PP.text msg]

instance Show Err where
    show e = PP.render $ ppErr e

instance E.Error Err where
    noMsg = Err (P.initialPos "") "Error"
    strMsg = Err (P.initialPos "")


{-
data Err = ArityErr Int [Val]
    | TypeErr ErrMsg Val
    | ParseErr P.ParseError
    | NotFunErr ErrMsg String -- msg, fname
    | UnboundIdentErr ErrMsg Key
    | EnvUpdateErr Key
    | BadExprErr ErrMsg Val
    | DefaultErr ErrMsg

ppErr (ArityErr expected found) = PP.text "ArityErr: expecting"
    <+> PP.int expected
    <+> PP.text "args, Found:"
    <+> (PP.fsep $ ppValList found)
ppErr (TypeErr expected found) = PP.text "TypeErr: expecting"
    <+> PP.text expected
    <+> PP.text ", Found:"
    <+> ppVal found
ppErr (ParseErr err) = PP.text "ParseErr:"
    <+> PP.text (show err)
ppErr (NotFunErr msg val) = PP.text "NotFunErr: not a function:"
    <+> PP.text msg
    <+> PP.text val
ppErr (UnboundIdentErr msg var) = PP.text msg
    <> PP.text ":"
    <+> PP.text var
ppErr (EnvUpdateErr var) = PP.text "destructive update on"
    <+> PP.text var
ppErr (BadExprErr msg val) = PP.text msg
    <> PP.text ":"
    <+> ppVal val
ppErr (DefaultErr msg) = PP.text msg
-}


