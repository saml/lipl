module LangData ( Val (..)
    , Err (..)
    , CanBeErr
    , Stack, pop, push
    , Queue, front ) where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<>), (<+>), ($$), ($+$) )

import qualified Text.ParserCombinators.Parsec as P

import qualified Control.Monad.Error as E

type Key = String
type KeyVal = (Key, Val)
type Env = [KeyVal]

ppKeyVal (k, v) = ppVal (Ident k)
    <+> PP.text "="
    <+> ppVal v

ppKeyValList = map ppKeyVal

data Val = Comment String
    | Null
    | Ident String
    | Int Integer
    | Float Double
    | Bool Bool
    | Char Char
    | Str String
    | Fun [String] Val -- params, body
    | List [Val]
    | Dict Env
    | Expr [Val]
    --deriving (Show)

instance Show Val where show = PP.render . ppVal

ppVal (Comment s) = PP.text ("#" ++ s)
ppVal Null = PP.text "Null"
ppVal (Ident s) = PP.text s
ppVal (Int i) = PP.integer i
ppVal (Float f) = PP.double f
ppVal (Bool b) = PP.text $ show b
ppVal (Char c) = PP.text $ show c
ppVal (Str s) = PP.text $ show s
ppVal (List xs) = PP.brackets
    (PP.hsep $ PP.punctuate PP.comma (ppValList xs))
ppVal (Dict xs) = PP.braces
    (PP.hsep $ PP.punctuate PP.comma (ppKeyValList xs))
ppVal (Expr xs) = PP.parens (PP.hsep $ ppValList xs)

ppValList = map ppVal

example = List [Ident "foo", Int 42, Char 'a'
    , List [Bool True, Str "Hello World", Float (-242.53)]
    , List [Ident "d", Dict [("bar", Int 24), ("f", Char 'c')]]]

data Err = ArityErr Int [Val]
    | TypeErr String Val
    | ParseErr P.ParseError
    | NotFunErr String String -- msg, fname
    | UnboundIdentErr String String
    | BadExprErr String Val
    | DefaultErr String

ppErr (ArityErr expected found) = PP.text "Expecting arity:"
    <+> PP.int expected
    <+> PP.text "Found:"
    <+> (PP.hsep $ ppValList found)
ppErr (TypeErr expected found) = PP.text "Expecting type:"
    <+> PP.text expected
    <+> PP.text "Found:"
    <+> ppVal found
ppErr (ParseErr err) = PP.text "Parse error:"
    <+> PP.text (show err)
ppErr (NotFunErr msg val) = PP.text "Not function:"
    <+> PP.text msg
    <+> PP.text val
ppErr (UnboundIdentErr msg var) = PP.text msg
    <> PP.text ":"
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

type CanBeErr = Either Err

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


