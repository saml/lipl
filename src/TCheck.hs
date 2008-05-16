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

tInfer :: (MonadTI m) => Val -> m Type
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
            fail ("not homogenious list: " ++ show (List l))

tInfer (Pair a b) = do
    tA <- tInfer a
    tB <- tInfer b
    s <- getSubst
    return $ pair (apply s tA) (apply s tB)

tInfer (Ident x) = do
    s <- getSubst
    case Map.lookup x s of
        Just ts -> do
            t <- toType ts
            s <- getSubst
            return (apply s t)
        otherwise -> fail (x ++ " not found in assumptions")

tInfer (Expr []) = return tUnit
tInfer (Expr l) = tInfer (foldl1 App l)

tInfer (App f x) = do
    tF <- tInfer f
    tX <- tInfer x
    v <- newTVar
    let tF' = tX `fn` v
    unify tF tF'
    s <- getSubst
    return (apply s v)

tInfer (If pred true false) = do
    tPred <- tInfer pred
    unify tPred tBool
    tTrue <- tInfer true
    tFalse <- tInfer false
    --sF <- mgu (apply s tTrue) (apply s tFalse)
    s <- getSubst
    unify (apply s tTrue) tFalse
    s <- getSubst
    return (apply s tFalse)
    {-
    s <- getSubst
    unify (apply s tTrue) (apply s tFalse)
    s' <- getSubst
    return $ apply s' tFalse
    -}

tInfer (Lambda [] e) = tInfer e

tInfer lam@(Lambda [x] e) = do
    v <- newTVar
    let sF = (x +-> TScheme [] v)
    tE <- localSubst sF (tInfer e)
    s <- getSubst
    let domain = apply s v
    let result = domain `fn` tE -- (apply s tE)
    return (apply s result)
-- apply [("t0", TScheme [] (tParse "Int -> t1")), ("t1", TScheme [] (tParse "Int -> t2"))] (tParse "t0")

tInfer lam@(Lambda params _) = if noDup params
    then
        tInfer (simplifyLambda lam)
    else
        fail $ "duplicate argument: " ++ (show lam)

tInfer (Let [(k,v)] e) = do
    tV <- tInfer v
    s <- getSubst
    let keys = Map.keys s
    let vals = Map.elems s
    let freeVs = (tv tV \\ keys) \\ tv vals

    s <- getSubst
    localSubst s (do
        extendSubst (k +-> TScheme freeVs tV)
        tE <- tInfer e
        sE <- getSubst
        tK <- tInfer (Ident k)
        sK <- getSubst
        --traceM ("tK: " ++ show tK)
        --traceM ("tV: " ++ show tV)
        --traceM ("sK: " ++ showS sK)
        unify tK tV

        s <- getSubst
        --traceM ("s: " ++ showS s)
        --putSubst $ Map.delete k s
        return (apply s tE))
    --return (s2 @@ s1, tE)
-- tyi "(let { x = (lambda (x) (+ 1 x)) } (x 1))"
-- tyi "(let { x = (lambda (x) (if (== 1 (head x)) 'a' 'b')), y = x} (y ))"

tInfer (Let kvs e) = tInfer $ foldr (Let . (:[])) e kvs

tInfer (Seq _ e2) = do
    tE2 <- tInfer e2
    return tE2

tInfer e@(FunDef name args body) = if noDup args
    then
        do
            v <- newTVar
            extendSubst (name +-> TScheme [] v)
            tF <- tInfer (Lambda args body)
            unify v tF
            s <- getSubst
            let result = apply s v
            extendSubst (name +-> mkPolyType result)
            return result
    else
        fail $ "duplicate argument: " ++ show e

typeInfer e@(FunDef name args body) = do
    t <- M.liftM tSanitize (locally (tInfer e))
    extendSubst (name +-> mkPolyType t)
    --s <- getSubst
    return t

typeInfer (Expr [e]) = typeInfer e
typeInfer e = do
    t <- locally (tInfer e)
    return (tSanitize t)

{-
ti val = do
    t <- tInfer val
    return (tSanitize t)
-}

--sSanitize = do
--    s <- getSubst



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


showS s = showSubst $ onlyNew s
onlyNew s = s `Map.difference` initialSubst

locally action = do
    s <- getSubst
    n <- getN
    result <- action
    putSubst s
    putN n
    return result

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



-- ti (Lambda ["x"] (Lambda ["x"] (App (App (PrimFun "+") (Ident "x")) (Ident "x"))))


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

clearSubst :: (MonadTI m) => m ()
clearSubst = putSubst initialSubst


showTI ti = let (s,_,t) = runTI ti initialSubst 0
    in
        showSubstTypePair (s,t)


