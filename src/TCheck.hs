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
import CoreLib
import Utils

type Assumpt = Subst

newtype SI a = SI { runSI :: Subst -> Int -> (Subst, Int, a) }
data TI a = TI (Subst -> Int -> (Subst, Int, a))
    | Fail String

--newtype TI a = TI { runTI :: E.ErrorT String SI a }
-- newtype TI a = TI { runTI :: Subst -> Int -> Either String (Subst, Int, a) }

{-
instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of
        (s', n', x) -> let TI gx = g x in gx s' n')
    Fail msg >>= _ = Fail msg
    fail = Fail
-}
instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of
        (s', n', x) -> case g x of
            TI gx -> gx s' n'
            Fail msg -> error msg
            {- Fail msg -> (s', n', x) -})
    Fail msg >>= _ = Fail msg
    fail = Fail


showTI ti = let (s,_,t) = runTI ti initialSubst 0
    in
        showSubstTypePair (s,t)

runTI (TI f) s i = f s i
-- runTI (Fail msg) s i = Fail msg

{-
instance S.MonadState (TI a) a where
    get = TI (\s n -> (s, n, (s,n)))
    put (s,n) = TI (\_ _ -> (s, n, ()))
-}

getSubst = TI (\s n -> (s, n, s))

getN = TI (\s n -> (s, n, n))

putSubst s = TI (\_ n -> (s, n, ()))

putN n = TI (\s _ -> (s, n, ()))

extendSubst s' = TI (\s n -> (s @@ s', n, ()))
-- showSubst ([("x", TScheme [] tInt)] @@ [("x", TScheme [] tChar)])
-- runTI (extendSubst [("x", TScheme [] tChar)]) [("x", TScheme [] tInt)] 0

unify t1 t2 = do
    s <- getSubst
    u <- mgu (apply s t1) (apply s t2)
    extendSubst u


newId = TI (\s n -> (s, n+1, "t" ++ show n))

newTVar = do
    v <- newId
    return (TVar v)

testNewTVar = runTI (sequence [newTVar, newTVar, newTVar]) nullSubst 0

find x assumptions = case lookup x assumptions of
    Just t -> return t
    otherwise -> fail $ "unbound type variable: " ++ x


fromIdType = map (\(k,v) -> (k, TScheme (tv v) v))

defaultSubst :: Subst
defaultSubst = fromIdType [
    ("Int", tInt)
    , ("Float", tFloat)
    , ("Bool", tBool)
    , ("Char", tChar)
    , ("Str", list tChar)
    ]

tInfer :: Val -> TI Type
tInfer (Int _) = return tInt
tInfer (Bool _) = return tBool
tInfer (Float _) = return tFloat
tInfer (Char _) = return tChar
tInfer (Str _) = return $ list tChar

tInfer (PrimFun x) = do
    s <- getSubst
    let Just (TScheme _ tX) = lookup x s
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
    case lookup x s of
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
    let keys = map fst s
    let vals = map snd s
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

{-
ti :: Val -> TI Type
ti e@(Let _ _) = local (tInfer e)
ti e@(Lambda _ _) = local (tInfer e)
ti e = tInfer e
-}

runTests = [
    ty "(let { f = (lambda (f) f) } ( (f 1), (f True)))"
        `tEq` tParse "(Int,Bool)"
    , ty "(let { x = (lambda (x) (+ 1 x)) } (x 1))"
        `tEq` tInt
    , ty "(let { x = (lambda (x) (if (== 1 (head x)) 'a' 'b')), y = x} (y ))"
        `tEq` tParse "[Int] -> Char"
    , ty "(lambda (f x) (f x))"
        `tEq` tParse "(t0 -> t1) -> t0 -> t1"
    , ty "(def fac (n) (if (<= n 0) 1 (* n (fac (- n 1)))))"
        `tEq` tParse "Int -> Int"
    , ty "(def twice (f x) (f (f x)))"
        `tEq` tParse "(t0 -> t0) -> t0 -> t0"
    , ty "(def len (l) (if (isEmpty l) 0 (+ 1 (len (tail l)))))"
        `tEq` tParse "[t0] -> Int"
    , ty "(def filter (f l) (if (f (head l)) (cons (head l) (filter f (tail l))) (filter f (tail l))))"
        `tEq` tParse "(t0 -> Bool) -> [t0] -> [t0]"
    , ty "(def map (f l) (cons (f (head l)) (map f (tail l))))"
        `tEq` tParse "(t0 -> t1) -> [t0] -> [t1]"
    , ty "(def map (f l) (let {x = (head l), xs = (map f (tail l))} (cons (f x) xs)))"
        `tEq` tParse "(t0 -> t1) -> [t0] -> [t1]"
    , ty "(lambda (l) (let {x = (head l), xs = (tail l)} (cons x xs)))"
        `tEq` tParse "[t0] -> [t0]"
    , ty "(lambda (l) (let {x = (head l), xs = (tail l)} (x,xs)))"
        `tEq` tParse "[t0] -> (t0, [t0])"
    , ty "((lambda (f x y) (f y x)) (lambda (x) x))"
        `tEq` tParse "b -> (b -> c) -> c"
    , ty "(lambda (x) (let {x = 1} x))"
        `tEq` tParse "a -> Int"
    , ty "(lambda (x) (lambda (x) (+ x x)))"
        `tEq` tParse "(t0 -> Int -> Int)"
    , ty "(def acc (f i l) (if (isEmpty l) i (f (head l) (acc f i (tail l)))))"
        `tEq` tParse "(t0 -> t1 -> t1) -> t1 -> [t0] -> t1"
    , ty "(lambda (f x y) (f y x))"
        `tEq` tParse "(a -> b -> c) -> b -> a -> c"
    ]

unifiable t1 t2 = case runTI action initialSubst 0 of
    (_,_,result) -> result
    where
        action = (do
            unify t1 t2
            s <- getSubst
            return (apply s t1 == apply s t2))

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

withinScope s action = do
    sOrig <- getSubst
    putSubst s
    result <- action
    putSubst sOrig
    return result

local action = do
    sOrig <- getSubst
    result <- action
    putSubst sOrig
    return result


onlyNew s = exclude s (map fst initialSubst)

localSubst s action = do
    sOrig <- getSubst
    let cache = (Map.toList $ Map.intersection (Map.fromList sOrig) (Map.fromList s))
    extendSubst s
    result <- action
    s' <- getSubst
    let cache' = map (\(k,v) -> (k, apply s' v)) cache
    let ks = keys s
    let s'' = exclude s' ks @@ cache
    putSubst s''
    return result
    where
        keys = map fst

    {-
    result <- action
    s' <- getSubst
    let ks = keys s
    putSubst (exclude s' ks)
    return result
    where
        keys = map fst
    -}

withSubst s action = do
    n <- getN
    sCurr <- getSubst
    --extendSubst s
    putSubst s
    result <- action
    putN n
    putSubst sCurr
    return result

withExtendSubst s action = do
    n <- getN
    sCurr <- getSubst
    extendSubst s
    result <- action
    putN n
    putSubst sCurr
    return result

eliminate ids = filter (\x -> fst x `notElem` ids)

tUpdateTVars e = do
    let vs = getTVars e
    vs' <- mapM (const newId) vs
    return $ subst (zip vs vs') e



{-
replaceIdent i i' e@(Ident x)
    | x == i = Ident i'
    | otherwise = e
replaceIdent
-}

simplifyLambda lam@(Lambda [] e) = lam
simplifyLambda lam@(Lambda [x] e) = lam
simplifyLambda lam@(Lambda (x:xs) e) =
    Lambda [x] (simplifyLambda (Lambda xs e))
simplifyLambda (Expr [x]) = simplifyLambda x

{-
substVal x y e@(Ident i) = if x == i then Ident y else e
substVal x y (FunDef name args e) = FunDef name args (substVal x y e)
substVal x y (Lambda args e) = Lambda args (substVal x y e)
substVal _ _ e = e

sanitizeVal (FunDef name args e) = FunDef name args (sanitizeVal e)
sanitizeVal lam@(Lambda [] _) = lam
sanitizeVal lam@(Lambda [x] e) = Lambda [x] (substVal x e)
--sanitizeVal lam@(Lambda args e)
sanitizeVal x = x
-}

substVal dict e@(Ident i) = case lookup i dict of
    Just i' -> Ident i'
    Nothing -> e
substVal dict (FunDef name args e) = FunDef name args (substVal dict e)
substVal dict (Lambda args e) = Lambda args (substVal dict e)
substVal dict v = v

sanitizeVal e =
    substVal [(v, v ++ show i) | (i,v) <- zip [0..] (idents e)] e


{-
allSameType l = and $
    zipWith (\a b -> I.runIdentity (valToType a)
        == I.runIdentity (valToType b)) l (tail l)
-}

tEq t1 t2 = tSanitize t1 == tSanitize t2

ty input = case parseSingle input of
    Right v -> case runTI (tInfer v) initialSubst 0 of
        (s,i,t) -> t --tSanitize t
    Left err -> error (show err)
    where
        prettySubst s = List.sort (exclude s (map fst initialSubst))


tyi input = case parseSingle input of
    Right v -> case runTI (tInfer v) initialSubst 0 of
        (s,i,t) -> putStrLn
            $ showSubstType (prettySubst s) t -- (tSanitize t)
    Left err -> putStrLn (show err)
    where
        prettySubst s = List.sort (exclude s (map fst initialSubst))
        -- (s,i,t) -> putStrLn $ showSubstType (s \\ initialSubst) t

tyim input = case parseMultiple "" input of
    Right l -> case runTI (tInferList l) initialSubst 0 of
        (s, i, t) -> mapM_ (putStrLn . show) (map tSanitize t)
    Left err -> putStrLn (show err)

initialSubst = defaultSubst `List.union` fromIdType builtinSubst

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
    --Fail msg -> Left msg
    --err -> Left (show err)
{-
rename dict e@(Ident x) = case lookup x dict of
    Just x' -> Ident x'
    Nothing -> e
rename dict (FunDef _ args e) = rename dict e

uniqueId = concat
renameLambda bvs (Lambda [x] e) = if x `elem` bvs
    then
        let x' = uniqueId bvs in Lambda [x'] (renameLambda (x':bvs) e)
    else
        Lambda [x] (renameLambda (x:bvs) e)
renameLambda bvs (Ident x) = if x `elem` bvs
    then
-}
