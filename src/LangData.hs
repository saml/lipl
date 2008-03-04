module LangData (Val (..)) where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<>), (<+>), ($$), ($+$) )

type Key = String
type KeyVal = (Key, Val)
type Env = [KeyVal]

ppKeyVal (k, v) = ppVal (Ident k)
    <+> PP.text "="
    <+> ppVal v

data Val = Comment String
    | Null
    | Ident String
    | Int Integer
    | Float Double
    | Bool Bool
    | Char Char
    | Str String
    | List [Val]
    | Dict Env
    | Expr [Val]

ppVal (Comment s) = PP.text ("#" ++ s)
ppVal (Ident s) = PP.text s
ppVal (Int i) = PP.integer i
ppVal (Float f) = PP.double f
ppVal (Bool b) = PP.text $ show b
ppVal (Char c) = PP.text $ show c
ppVal (Str s) = PP.text $ show s
ppVal (List xs) = PP.brackets
    (PP.hsep $ PP.punctuate PP.comma (map ppVal xs))
ppVal (Dict xs) = PP.braces
    (PP.hsep $ PP.punctuate PP.comma (map ppKeyVal xs))
ppVal (Expr xs) = PP.parens (PP.hsep $ map ppVal xs)

instance Show Val where show = PP.render . ppVal

example = List [Ident "foo", Int 42, Char 'a'
    , List [Bool True, Str "Hello World", Float (-242.53)]
    , List [Ident "d", Dict [("bar", Int 24), ("f", Char 'c')]]]
