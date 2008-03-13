module LangData ( Val (..)
    , PrimFun (..)
    , Err (..)
    , EvalVal, runEvalVal
    , emptyEnv, putKeyVals
    , Stack, pop, push
    , Queue, front ) where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<>), (<+>), ($$), ($+$) )

import qualified Text.ParserCombinators.Parsec as P

import qualified Control.Monad.Error as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Reader as R

import qualified Data.Map as Map

type Key = String
type KeyVal = (Key, Val)
type EnvList = [KeyVal]
type Env = Map.Map String Val

putKeyVal (key, val) env = if Map.member key env
    then
        E.throwError $ EnvUpdateErr key
    else
        return $ Map.insert key val env

toEnvList keys vals = zip keys vals

putKeyVals keys vals env = Map.union env
    $ Map.fromList $ toEnvList keys vals

emptyEnv = Map.empty

ppKeyVal (k, v) = ppVal (Ident k)
    <+> PP.text "="
    <+> ppVal v

ppKeyValList = map ppKeyVal

data Val = Comment String
    | Null
    | Ident { unpackIdent :: String }
    | Int { unpackInt :: Integer }
    | Float { unpackFloat :: Double }
    | Bool { unpackBool :: Bool }
    | Char { unpackChar :: Char }
    | Str { unpackStr :: String }
    -- | environment, name, params, body
    | Fun Env String [String] Val
    -- | primitive function: name, func
    | Prim String PrimFun
    | List [Val]
    | Dict Env
    | Expr [Val]
    deriving (Ord, Eq)
--    deriving (Show)

instance Show Val where show = PP.render . ppVal

newtype PrimFun = PrimFun ([Val] -> EvalVal Val)
instance Ord PrimFun where
    compare a b = LT
instance Eq PrimFun where
    a == b = False


ppVal (Comment s) = PP.text ("#" ++ s)
ppVal Null = PP.text "Null"
ppVal (Ident s) = PP.text s
ppVal (Int i) = PP.integer i
ppVal (Float f) = PP.double f
ppVal (Bool b) = PP.text $ show b
ppVal (Char c) = PP.text $ show c
ppVal (Str s) = PP.text $ show s
ppVal (Fun env name args body) = PP.parens
    $ PP.hsep [PP.text "def", PP.text name, ppArgs args, ppVal body]
ppVal (Prim name body) = PP.parens
    $ PP.hsep [PP.text "\\def", PP.text name]
ppVal (List xs) = PP.brackets
    (PP.hsep $ PP.punctuate PP.comma (ppValList xs))
ppVal (Dict xs) = PP.braces
    (PP.hsep $ PP.punctuate PP.comma (ppKeyValList (Map.toList xs)))
ppVal (Expr xs) = PP.parens (PP.hsep $ ppValList xs)

ppValList = map ppVal
ppStrList = map PP.text
ppArgs args = PP.parens $ PP.hsep $ ppStrList args

example = List [Ident "foo", Int 42, Char 'a'
    , List [Bool True, Str "Hello World", Float (-242.53)]
    , List [Ident "d"
        , Dict $ Map.fromList [("bar", Int 24), ("f", Char 'c')]]]

data Err = ArityErr Int [Val]
    | TypeErr String Val
    | ParseErr P.ParseError
    | NotFunErr String String -- msg, fname
    | UnboundIdentErr String String
    | EnvUpdateErr String
    | BadExprErr String Val
    | DefaultErr String

ppErr (ArityErr expected found) = PP.text "ArityErr: expecting"
    <+> PP.int expected
    <+> PP.text "args, Found:"
    <+> (PP.hsep $ ppValList found)
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

instance Show Err where
    show e = PP.render $ ppErr e

instance E.Error Err where
    noMsg = DefaultErr "Error"
    strMsg = DefaultErr


--type EvalVal a = E.ErrorT Err I.Identity a
--runEvalVal = I.runIdentity . E.runErrorT
type EvalVal a = R.ReaderT Env (E.ErrorT Err I.Identity) a
runEvalVal env e = (I.runIdentity . E.runErrorT) $ R.runReaderT e env

type Stack = [Val]
type Queue = [Val]

pop :: Stack -> (Val, Stack)
pop (x:xs) = (x, xs)
pop _ = (Null, [])

push :: Val -> Stack -> Stack
push v s = v : s

front :: Queue -> (Val, Queue)
front (x:xs) = (x, xs)
front _ = (Null, [])

-- | evaluation using 'Stack' and 'Queue'
--sqEval :: Stack -> Queue -> Stack
--sqEval s (Ident fname : xs) =

--f :: Stack -> Queue -> Val


