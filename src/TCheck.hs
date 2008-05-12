{-# LANGUAGE TemplateHaskell #-}

module TCheck where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.List ((\\))
import qualified Control.Monad as M
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as T
import Debug.Trace (trace)

import LangData
import Parser (parse, parseSingle, parseMultiple)
import Type
import TParse
import Trace
import CoreLib (builtinSubst)
import Utils
import TIMonad

tInfer :: Val -> TI Type
tInfer (Int _) = return tInt
tInfer (Bool _) = return tBool
tInfer (Float _) = return tFloat
tInfer (Char _) = return tChar
tInfer (Str _) = return $ list tChar

tInfer (PrimFun x) = do
    s <- getSubst
    let Just (TScheme _ tX) = Map.lookup x s
    tX' <- tUpdateTVars tX
    return tX'

tInfer (List []) = do
    v <- newTVar
    return $ list v

tInfer (List l) = do
    ts <- mapM tInfer l
    if allEq ts
        then
            return (list (head ts))
        else
            fail "not homogenious list"

tInfer (Pair a b) = do
    tA <- tInfer a
    tB <- tInfer b
    s <- getSubst
    --traceM ("Pair: s:" ++ showSubst (exclude s (map fst initialSubst)))
    return $ pair (apply s tA) (apply s tB)

tInfer (Ident x) = do
    s <- getSubst
    case Map.lookup x s of
        Just ts -> do
            t <- toType ts
            return t
        otherwise -> fail (x ++ " not found in assumptions")

tInfer (Expr []) = return tUnit
tInfer (Expr l) = tInfer (foldl1 App l)

tInfer (App f x) = do
    tF <- tInfer f
    tX <- tInfer x
    s' <- getSubst
    v <- newTVar
    let tF' = tX `fn` v
    unify tF tF'
    s <- getSubst
    let result = apply s v
    return result

tInfer (If pred true false) = do
    tPred <- tInfer pred
    unify tPred tBool
    tTrue <- tInfer true
    tFalse <- tInfer false
    s <- getSubst
    unify (apply s tTrue) tFalse
    s' <- getSubst
    return $ apply s' tFalse

tInfer (Lambda [] e) = tInfer e

tInfer lam@(Lambda [x] e) = do
    v <- newTVar
    let sF = (x +-> TScheme [] v)
    tE <- localSubst sF (tInfer e)
    s <- getSubst
    let domain = apply s v
    let result = domain `fn` tE -- (apply s tE)
    return result
    where
        getVar x s = case lookup x s of
            Just t -> t
            otherwise -> TScheme [] tUnit
-- apply [("t0", TScheme [] (tParse "Int -> t1")), ("t1", TScheme [] (tParse "Int -> t2"))] (tParse "t0")

tInfer lam@(Lambda params _) = if noDup params
    then
        tInfer (simplifyLambda lam)
    else
        fail "duplicate argument"

tInfer (Let [(k,v)] e) = do
    tV <- tInfer v
    s <- getSubst
    let keys = Map.keys s
    let vals = Map.elems s
    let freeVs = (tv tV \\ keys) \\ tv vals

    extendSubst (k +-> TScheme freeVs tV)
    tE <- tInfer e
    sE <- getSubst
    tK <- tInfer (Ident k)
    sK <- getSubst
    unify tK tV

    s <- getSubst
    return (apply s tE)
    --return (s2 @@ s1, tE)
-- tyi "(let { x = (lambda (x) (+ 1 x)) } (x 1))"
-- tyi "(let { x = (lambda (x) (if (== 1 (head x)) 'a' 'b')), y = x} (y ))"

tInfer (Let kvs e) = tInfer $ foldr (Let . (:[])) e kvs

tInfer (FunDef name args body) = if noDup args
    then
        do
            v <- newTVar
            extendSubst (name +-> TScheme [] v)
            tF <- tInfer (Lambda args body)
            unify tF v
            s <- getSubst
            return (apply s tF)
    else
        fail "duplicate argument"

{-
tInfer (Let kvs e) = tInfer $ Expr (Lambda (keys kvs) e : vals kvs)
    where
        keys = map fst
        vals = map snd
-- Let [(x1, e1), (x2,e2), ..., (xN,eN)] expr
-- ==> Expr [Lambda [x1, x2, ..., xN] expr, e1, e2, ...]
-}




mkFunType [] = do
    v <- newTVar
    return v

mkFunType (_:xs) = do
    v <- newTVar
    rest <- mkFunType xs
    return (v `fn` rest)


tInferList :: [Val] -> TI [Type]
tInferList = mapM tInfer


toType (TScheme [] t) = return t

toType (TScheme (x:xs) t) = do
    v <- newId
    toType $ TScheme xs (replace x v t)


onlyNew s = exclude s (Map.keys initialSubst)

localSubst s action = do
    sOrig <- getSubst
    let cache = sOrig `Map.intersection` s
    extendSubst s
    result <- action
    s' <- getSubst
    let cache' = Map.map (apply s') cache
    let ks = Map.keys s
    let s'' = subtractMap s' ks @@ cache
    putSubst s''
    return result

tUpdateTVars e = do
    let vs = getTVars e
    vs' <- mapM (const newId) vs
    return $ subst (zip vs vs') e



simplifyLambda lam@(Lambda [] e) = lam
simplifyLambda lam@(Lambda [x] e) = lam
simplifyLambda lam@(Lambda (x:xs) e) =
    Lambda [x] (simplifyLambda (Lambda xs e))
simplifyLambda (Expr [x]) = simplifyLambda x


ty input = case parseSingle input of
    Right v -> case runTI (tInfer v) initialSubst 0 of
        (s,i,t) -> t --tSanitize t
    Left err -> error (show err)
    where
        prettySubst s = s `Map.difference` initialSubst


tyi input = case parseSingle input of
    Right v -> case runTI (tInfer v) initialSubst 0 of
        (s,i,t) -> putStrLn
            $ showSubstType (prettySubst s) t -- (tSanitize t)
    Left err -> putStrLn (show err)
    where
        prettySubst s = s `Map.difference` initialSubst
        -- (s,i,t) -> putStrLn $ showSubstType (s \\ initialSubst) t

tyim input = case parseMultiple "" input of
    Right l -> case runTI (tInferList l) initialSubst 0 of
        (s, i, t) -> mapM_ (putStrLn . show) (map tSanitize t)
    Left err -> putStrLn (show err)



ti val = let (_,_,t) = runTI (tInfer val) initialSubst 0
    in
        t
-- ti (Lambda ["x"] (Lambda ["x"] (App (App (PrimFun "+") (Ident "x")) (Ident "x"))))


traceM msg = if isDebugSet
    then
        trace ('\n' : msg) (return ())
    else
        return ()
    where
        isDebugSet = False

printRunTIResult (s,i,t) = putStrLn $ show t

theta = [("X", TVar "a"), ("Y", TVar "b"), ("Z", TVar "Y")]
eta = [("X", TApp (TVar "f") (TVar "Y")), ("Y", TVar "Z")]
-- theta @@ eta = [("X",f b),("Z",Y)]

tCheck :: Val -> Either String Type
tCheck v = case runTI (tInfer v) initialSubst 0 of
    (s, i, t) -> Right t


defaultSubst :: Subst
defaultSubst = toSubst [
    ("Int", tInt)
    , ("Float", tFloat)
    , ("Bool", tBool)
    , ("Char", tChar)
    , ("Str", list tChar)
    ]
initialSubst = defaultSubst `Map.union` toSubst builtinSubst


showTI ti = let (s,_,t) = runTI ti initialSubst 0
    in
        showSubstTypePair (s,t)


