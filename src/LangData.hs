{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LangData ( Val (..)
--    , PrimFun (..)
    , Err (..)
--    , Eval (..)
    , Wrap, runWrap, putVal, getVal, getEnv, pushEnv, popEnv
    , EnvStack
--    , emptyEnv, putKeyVals
    , Stack, pop, push
    , Queue, front ) where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<>), (<+>), ($$), ($+$) )

import qualified Text.ParserCombinators.Parsec as P

import qualified Control.Monad.Error as E
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Trans as T
import qualified Control.Monad.State as S

import qualified Data.Map as Map

type Key = String
type KeyVal = (Key, Val)
type EnvList = [KeyVal]
type Env = Map.Map Key Val

{-
putKeyVal (key, val) env = if Map.member key env
    then
        E.throwError $ EnvUpdateErr key
    else
        return $ Map.insert key val env

toEnvList keys vals = zip keys vals

putKeyVals keys vals env = Map.union env
    $ Map.fromList $ toEnvList keys vals

emptyEnv = Map.empty
-}

ppKeyVal (k, v) = ppVal (Ident k)
    <+> PP.text "="
    <+> ppVal v

ppKeyValList = map ppKeyVal

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
    | Fun Env Name Params Val
    -- | primitive function: name, func
    | PrimFun Name
    | Prim Name RemainingArgs [Val]
    | List [Val]
    | Dict Env
    | Expr [Val]
    | Let Env Val
    | Closure Val Env
    | Partial String Int [Val] -- func name, args needed, args have
    | If Val Val Val -- pred, if case, else case
    deriving (Ord, Eq)
--    deriving (Show)

instance Show Val where show = PP.render . ppVal

{-
newtype PrimFun = PrimFun ([Val] -> EvalVal Val)
instance Ord PrimFun where
    compare a b = LT
instance Eq PrimFun where
    a == b = False
-}

ppVal (Comment s) = PP.text ("#" ++ s)
ppVal Null = PP.text "Null"
ppVal (Ident s) = PP.text s
ppVal (Int i) = PP.integer i
ppVal (Float f) = PP.double f
ppVal (Bool b) = PP.text $ show b
ppVal (Char c) = PP.text $ show c
ppVal (Str s) = PP.text $ show s
ppVal (Fun env name args body) = PP.parens
    $ PP.hsep [PP.text "function"
        , PP.text name, ppArgs args, ppVal body]
ppVal (PrimFun name) = PP.text name
ppVal (Prim name remaining params) = PP.parens
    $ PP.hsep [PP.text "builtin-function"
        , PP.text name, PP.int remaining
        , PP.parens $ PP.hsep $ ppValList params]
ppVal (List xs) = PP.brackets
    (PP.hsep $ PP.punctuate PP.comma (ppValList xs))
ppVal (Dict xs) = PP.braces
    (PP.hsep $ PP.punctuate PP.comma (ppKeyValList (Map.toList xs)))
ppVal (Expr xs) = PP.parens (PP.hsep $ ppValList xs)
ppVal (If pred ifCase elseCase) = PP.parens
    $ PP.hsep [PP.text "if", ppVal pred, ppVal ifCase, ppVal elseCase]
ppVal (FunDef name args body) = PP.parens
    $ PP.hsep [PP.text "def", PP.text name
        , ppArgs args
        , ppVal body]

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
    | UnboundIdentErr String Key
    | EnvUpdateErr Key
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
--type EvalVal a = R.ReaderT Env (E.ErrorT Err I.Identity) a
type EnvStack = Stack Env

getEnv :: Wrap Env
getEnv = do
    st <- S.get
    return $ (fst . pop) st

{-
type ValI = I.Identity
type ValS = S.StateT EnvStack ValI
type ValE = E.ErrorT Err ValS
-}

newtype Wrap a = Wrap {
    runWrap :: E.ErrorT Err (S.StateT EnvStack IO) a
} deriving (
    Functor, Monad, E.MonadError Err, S.MonadState EnvStack, T.MonadIO)

{-
instance (Show a) => Show Wrap a where
    show wrap = S.runErrorT (S.runStateT (runWrap wrap) [])
-}
{-
unwrap wrap = case fst $ (
        I.runIdentity $ S.runStateT (E.runErrorT $ runWrap wrap) []) of
    Left err -> Null
    Right val -> val
-}

putVal key val = do
    (st:xs) <- S.get
    if Map.member key st
        then
            E.throwError $ EnvUpdateErr key
        else
            S.put (Map.insert key val st:xs)

getVal key = do
    (st:xs) <- S.get
    case Map.lookup key st of
        Just val -> return val
        otherwise -> E.throwError $ UnboundIdentErr "not found" key

pushEnv :: Env -> Wrap ()
pushEnv env = do
    st <- S.get
    S.put (push env st)

popEnv :: Wrap ()
popEnv = do
    st <- S.get
    S.put $ (snd . pop) st

--runWrap env a = I.runIdentity (S.runStateT (E.runErrorT a) env)

--runEval :: EnvStack -> Wrap Val -> (Either Err Val, EnvStack)
--runEval env val = I.runIdentity (S.runStateT (E.runErrorT val) env)

{-
newtype Eval a = Eval (Wrap a)
    deriving (Functor, Monad)
-}
--type Val' = Eval Val
--type EvaluatedVal = (Either Err Val, EnvStack)
--runEval :: EnvStack -> Val' -> (Either Err Val, EnvStack)
--runEval :: EnvStack -> Eval Val -> (Either Err Val, EnvStack)

type Stack a = [a]
type Queue a = [a]

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x, xs)

push :: a -> Stack a -> Stack a
push v s = v : s

front :: Queue a -> (a, Queue a)
front (x:xs) = (x, xs)


