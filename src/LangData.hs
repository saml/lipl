{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LangData ( Val (..)
    , Err (..)
    , Wrap, runWrap, putVal, updateVal, getVal
    , getEnv, getEnvFor
    , pushEnv, popEnv, clearEnv, showEnv, extendPushEnv
    , nullEnv, emptyEnv
    , EnvStack
    , KeyValList, Key
    , Stack, pop, push
    , Queue, front ) where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<>), (<+>), ($$), ($+$) )

import qualified Text.ParserCombinators.Parsec as P

import qualified Control.Monad.Fix as F
import qualified Control.Monad.Error as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Trans as T
import qualified Control.Monad.State as S
import qualified Control.Monad as M
import qualified Control.Applicative as A
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Debug.Trace (trace)

type Key = String
type KeyVal = (Key, Val)
type KeyValList = [KeyVal]
type Env = Map.Map Key Val

type EnvStack = Stack Env

{-
data EnvStack = EnvStack {
    globalEnv :: Env
    , envStack :: Stack Env
    } deriving (Show)

ppEnvStack (EnvStack _ (x:xs)) = ppEnv env

instance Show EnvStack where
    show e = PP.render $ ppEnvStack e
-}

emptyEnv = Map.empty
nullEnv = [emptyEnv]

showEnv env = show $ ppEnv env

type Name = String
type Params = [Name]
type RemainingArgs = Int

data Val = Comment String
    | Null
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
    | Expr [Val]
    | Let KeyValList Val
    | Closure Env Val
    | If Val Val Val -- pred, if case, else case
    deriving (Ord, Eq)

instance Show Val where show = PP.render . ppVal

ppKeyVal (k, v) = ppVal (Ident k)
    <+> PP.text "="
    <+> ppVal v

ppKeyValList :: KeyValList -> PP.Doc
ppKeyValList l = ppDict $ map ((PP.empty $$) . ppKeyVal) l

ppDict l = PP.braces $ PP.fsep $ PP.punctuate PP.comma l

ppVal (Comment s) = PP.text ("#" ++ s)
ppVal Null = PP.text "Null"
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
--ppVal (Fun _ _ _) = PP.text "function"
--ppVal (Lambda _ _) = PP.text "lambda"
ppVal (PrimFun name) = PP.text name
ppVal (Prim name remaining params) = PP.parens
    $ PP.fsep [PP.text "builtin-function"
        , PP.text name, PP.int remaining
        , PP.parens $ PP.fsep $ ppValList params]
--ppVal (Prim name _ _) = PP.text name
ppVal (List xs) = PP.brackets
    (PP.fsep $ PP.punctuate PP.comma (ppValList xs))
ppVal (Dict xs) = ppKeyValList xs
--ppVal (Dict xs) = PP.braces
--    (PP.hsep $ PP.punctuate PP.comma (ppKeyValList (Map.toList xs)))
ppVal (Expr xs) = PP.parens (PP.fsep $ ppValList xs)
ppVal (If pred ifCase elseCase) = PP.parens
    $ PP.fsep [PP.text "if", ppVal pred, ppVal ifCase, ppVal elseCase]
ppVal (FunDef name args body) = PP.parens
    $ PP.fsep [PP.text "def", PP.text name
        , ppArgs args
        , ppVal body]
ppVal (Let env val) = PP.parens
    $ PP.fsep [PP.text "let", ppKeyValList env, ppVal val]
--ppVal (Closure env val) = PP.parens
--    $ PP.fsep [PP.text "closure", ppEnv env, ppVal val]

ppEnv :: Env -> PP.Doc
ppEnv env = ppKeyValList  $ Map.toList env

ppValList = map ppVal
ppStrList = map PP.text
ppArgs args = PP.parens $ PP.fsep $ ppStrList args

data Err = ArityErr Int [Val]
    | TypeErr String Val
    | ParseErr P.ParseError
    | NotFunErr String String -- msg, fname
    | UnboundIdentErr String Key
    | EnvUpdateErr Key
    | BadExprErr String Val
    | DefaultErr String

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

instance Show Err where
    show e = PP.render $ ppErr e

instance E.Error Err where
    noMsg = DefaultErr "Error"
    strMsg = DefaultErr


getEnv :: Wrap Env
getEnv = do
    st <- S.get
    return $ (fst . pop) st

getEnvFor :: [Key] -> Wrap Env
getEnvFor keys = do
    vals <- mapM getVal keys
    let env = Map.fromList $ zip keys vals
    return env
    --env <- getEnv
    --let keysEnv = Map.fromList $ map (\k -> (k, Null)) keys
    --return $ env `Map.intersection` keysEnv

newtype Wrap a = Wrap {
    runWrap :: E.ErrorT Err (S.StateT EnvStack IO) a
} deriving (
    Functor, Monad, F.MonadFix
    , E.MonadError Err, S.MonadState EnvStack, T.MonadIO)

instance A.Applicative Wrap where
    pure = return
    (<*>) = M.ap

putVal key val = do
    (st:xs) <- S.get
    if Map.member key st
        then
            E.throwError $ EnvUpdateErr key
        else
            S.put (Map.insert key val st:xs)

updateVal key val = do
    (env:xs) <- S.get
    S.put (Map.insert key val env : xs)

getVal key = do
    envs <- S.get
    case (catMaybes $ map (Map.lookup key) envs) of
        (x:_) -> return x
        otherwise -> E.throwError $ UnboundIdentErr "not found" key

{-
getVal key = do
    (env:xs) <- S.get
    case Map.lookup key env of
        Just val -> return val
        otherwise -> E.throwError $ UnboundIdentErr "not found" key
-}

pushEnv :: Env -> Wrap ()
pushEnv env = do
    st <- S.get
    S.put (push env st)

popEnv :: Wrap ()
popEnv = do
    st <- S.get
    if nullEnv == st
        then return ()
        else S.put $ (snd . pop) st

clearEnv :: Wrap ()
clearEnv = do
    S.put nullEnv

extendPushEnv :: Env -> Wrap ()
extendPushEnv env = do
    envs <- S.get
    if isEmpty envs
        then
            S.put (push env envs)
        else
            do
                let currEnv = head envs
                let newEnvs = push (env `Map.union` currEnv) envs
                S.put newEnvs

type Stack a = [a]
type Queue a = [a]

isEmpty :: Stack a -> Bool
isEmpty = null

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x, xs)
--pop [] = (0,[])

push :: a -> Stack a -> Stack a
push v s = v : s

front :: Queue a -> (a, Queue a)
front (x:xs) = (x, xs)
