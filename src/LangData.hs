module LangData ( Val (..)
    , Env, nullEnv, emptyEnv, EnvStack, showEnv, showEnvs
    , KeyValList, Key
    , ppValList, ppVal, toStr
    ) where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ ( (<>), (<+>), ($$), ($+$) )
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.Map as Map
import qualified Data.List as List

import Stack

type Key = String
type KeyVal = (Key, Val)
type KeyValList = [KeyVal]

ppKeyVal (k, v) = ppVal (Ident k)
    <+> PP.text "="
    <+> ppVal v

ppKeyValList :: KeyValList -> PP.Doc
ppKeyValList l = ppDict $ map ((PP.empty $$) . ppKeyVal) l
ppDict l = PP.braces $ PP.fsep $ PP.punctuate PP.comma l


type Env = Map.Map Key Val
type EnvStack = Stack Env
emptyEnv = Map.empty
nullEnv = [emptyEnv]

showEnv env = show $ ppEnv env
showEnvs envs = PP.render
    ((PP.fsep $ map ppEnv envs) $+$ PP.int (length envs))
ppEnv env = ppKeyValList  $ Map.toList env

ppValList = map ppVal
ppStrList = map PP.text
ppArgs args = PP.parens $ PP.fsep $ ppStrList args

type Name = String
type Params = [Name]
type RemainingArgs = Int

data Val = -- Comment String
    Null
    | At P.SourcePos Val
    | Seq Val Val
    | Ident { unpackIdent :: Name }
    | Int { unpackInt :: Integer }
    | Float { unpackFloat :: Double }
    | Bool { unpackBool :: Bool }
    | Char { unpackChar :: Char }
    | Str { unpackStr :: String }
    | FunDef Name Params Val
    | Lambda Params Val
    | Fun Env Params Val  -- parser don't generate this
    | PrimFun Name
    | Prim Name RemainingArgs [Val] -- parser don't generate this
    | List [Val]
    | Dict KeyValList
    | Pair Val Val
    | Expr [Val]
    | App Val Val
    | Let KeyValList Val
    | Closure KeyValList Val
    | If Val Val Val -- pred, if case, else case
    deriving (Ord, Eq)

instance Show Val where show = PP.render . ppVal

-- ppVal (Comment s) = PP.text ("#" ++ s)
ppVal Null = PP.text "Null"
ppVal (At _ e) = ppVal e
ppVal (Seq e1 e2) = ppVal e1 <+> ppVal e2
ppVal (Ident s) = PP.text s
ppVal (Int i) = PP.integer i
ppVal (Float f) = PP.double f
ppVal (Bool b) = PP.text $ show b
ppVal (Char c) = PP.text $ show c
ppVal (Str s) = PP.text $ show s
ppVal (Fun env args body) = PP.parens
    $ PP.fsep [PP.text "function"
        , ppArgs args, ppVal body]
ppVal (Lambda args body) = PP.parens
    $ PP.fsep [PP.text "lambda", ppArgs args, ppVal body]
ppVal (FunDef name args body) = PP.parens
    $ PP.fsep [PP.text "def", PP.text name
        , ppArgs args
        , ppVal body]
ppVal (PrimFun name) = PP.text name
ppVal (Prim name _ _) = PP.text "<builtin:" <+> PP.text name <> PP.text ">"
--ppVal (Fun _ _ _) = PP.text "<function>"
--ppVal (Lambda _ _) = PP.text "<lambda>"
--ppVal (Prim name _ _) = PP.text "<builtin>"
--ppVal (FunDef name _ _) = PP.text "<fundef>"
{-
ppVal (PrimFun name) = PP.text name
    $ PP.fsep [PP.text "builtin-function"
        , PP.text name, PP.int remaining
        , PP.parens $ PP.fsep $ ppValList params]
-}
ppVal (List l@(Char c:xs)) = ppVal (toStr l)
ppVal (List xs) = PP.brackets
    (PP.fsep $ PP.punctuate PP.comma (ppValList xs))
ppVal (Dict xs) = ppKeyValList xs
ppVal (Pair a b) = PP.parens
    (PP.fsep $ PP.punctuate PP.comma [ppVal a, ppVal b])
--ppVal (Dict xs) = PP.braces
--    (PP.hsep $ PP.punctuate PP.comma (ppKeyValList (Map.toList xs)))
ppVal (Expr xs) = PP.parens (PP.fsep $ ppValList xs)
ppVal (If pred ifCase elseCase) = PP.parens
    $ PP.fsep [PP.text "if", ppVal pred, ppVal ifCase, ppVal elseCase]
ppVal (Let env val) = PP.parens
    $ PP.fsep [PP.text "let", ppKeyValList env, ppVal val]
ppVal (App f x) = PP.parens (PP.fsep [ppVal f, ppVal x])
--ppVal (Closure env val) = PP.parens
--    $ PP.fsep [PP.text "closure", ppEnv env, ppVal val]

toStr [] = Str ""
toStr (Char c:xs) = let Str xs' = toStr xs
    in
        Str (c:xs')
