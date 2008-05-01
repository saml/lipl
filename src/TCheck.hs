{-# LANGUAGE TemplateHaskell #-}

module TCheck where

import qualified Data.List as List
import Data.List ((\\))
import qualified Control.Monad as M
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as T
import Debug.Trace (trace)

import LangData
import Parser (parseSingle, parseMultiple)
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

extendSubst s' = TI (\s n -> (s' @@ s, n, ()))

unify t1 t2 = do
    s <- getSubst
    u <- mgu (apply s t1) (apply s t2)
    extendSubst u


newId = TI (\s n -> (s, n+1, "v" ++ show n))

newTVar = do
    v <- newId
    return (TVar v)

testNewTVar = runTI (sequence [newTVar, newTVar, newTVar]) nullSubst 0

find x assumptions = case lookup x assumptions of
    Just t -> return t
    otherwise -> fail $ "unbound type variable: " ++ x


fromIdType = map (\(k,v) -> (k, TScheme [] v))

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
    return tX

tInfer (List []) = do
    v <- newTVar
    return $ list v

tInfer (List l@(x:xs)) = do
    ts <- mapM tInfer l
    if allEq ts
        then
            return (list (head ts))
        else
            fail "not homogenious list"

tInfer (Pair a b) = do
    tA <- tInfer a
    tB <- tInfer b
    return $ pair tA tB

tInfer (Ident x) = do
    s <- getSubst
    case lookup x s of
        Just ts -> do
            --traceM ("ID: ts: " ++ show ts)
            t <- toType ts
            --traceM ("ID: t: " ++ show t)
            return t
        otherwise -> fail (x ++ " not found in assumptions")

tInfer (Expr []) = return tUnit
tInfer (Expr [e]) = tInfer e
tInfer (Expr l) = tInfer (foldl1 App l)

tInfer (App f x) = do
    tF <- tInfer f
    tX <- tInfer x
    --s <- getSubst
    v <- newTVar
    --let tF_orig = apply s tF
    let tF_target = tX `fn` v
    unify tF tF_target
    --result <- mgu tF_orig tF_target
    --extendSubst result
    result <- getSubst
    return $ apply result v

tInfer (If pred trueCase falseCase) = do
    tPred <- tInfer pred
    sPred <- mgu tPred tBool
    tTrueCase <- tInfer trueCase
    tFalseCase <- tInfer falseCase
    sFalse <- getSubst
    sTrueFalse <- mgu (apply sFalse tTrueCase) tFalseCase
    return $ apply sTrueFalse tFalseCase

tInfer (Lambda [] e) = tInfer e

tInfer (Lambda [p] e) = do
    v <- newTVar
    let sPV = (p +-> TScheme [] v)
    --traceM ("LAMBDA: sPV: " ++ showSubst sPV)
    extendSubst sPV
    tE <- tInfer e
    --traceM ("LAMBDA: tE: " ++ show tE)
    s <- getSubst
    --traceM ("LAMBDA:" ++ showSubst (s \\ initialSubst))
    putSubst $ exclude s [p]
    let domain = apply s v
    --traceM ("LAMBDA: domain: " ++ show domain)
    let result = domain `fn` tE
    --traceM ("LAMBDA: result: " ++ show result)
    return result
-- tyi "(let { f = (lambda (f) f) } ( (f 1), (f True)))"


tInfer lam@(Lambda params _) = if noDup params
    then
        tInfer (simplifyLambda lam)
    else
        fail "duplicate argument"

tInfer (Let [(k,v)] e) = do
    --v' <- newTVar
    --let kv' = (k +-> TScheme [] v')
    --extendSubst kv'
    tV <- tInfer v
    --s <- mgu v' tV
    extendSubst (k +-> TScheme (tv tV) tV)
    --extendSubst (k +-> TScheme (tv tV) tV)
    tE <- tInfer e
    --s <- getSubst
    --traceM ("LET:" ++ showSubst (s \\ initialSubst))
    return tE
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
            s <- mgu v tF
            extendSubst (name +-> TScheme [] (apply s tF))
            return tF
    else
        fail "duplicate argument"
-- tyi "(def map (f l) (cons (f (head l)) (map f (tail l))))"
-- tyi "(def map (f l) (let {x = (head l), xs = (map f (tail l))} (cons (f x) xs)))"
-- tyi "(lambda (f x) (f x))"
-- tyi "(let {map = (lambda (f l) (let {x = (head l), xs = (map f (tail l))} (cons (f x) xs)))} map)"

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

tInferList :: [Val] -> TI [Type]
tInferList = mapM tInfer


toType (TScheme [] t) = return t

toType (TScheme (x:xs) t) = do
    v <- newId
    toType $ TScheme xs (replace x v t)

local action = do
    sOrig <- getSubst
    result <- action
    putSubst sOrig
    return result

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
allSameType l = and $
    zipWith (\a b -> I.runIdentity (valToType a)
        == I.runIdentity (valToType b)) l (tail l)
-}

{-
ty s = case parseSingle s of
    Right v -> putStrLn
        $ showSubstTypePair $ runTI
        $ tCheck (defaultSubst `List.union` builtinSubst) v
    otherwise -> error "parse error"
-}

tyi input = case parseSingle input of
    Right v -> case runTI (tInfer v) initialSubst 0 of
        (s,i,t) -> putStrLn
            $ showSubstType (prettySubst s) t
    Left err -> putStrLn (show err)
    where
        prettySubst s = List.sort (exclude s (map fst initialSubst))
        -- (s,i,t) -> putStrLn $ showSubstType (s \\ initialSubst) t

tyim input = case parseMultiple "" input of
    Right l -> case runTI (tInferList l) initialSubst 0 of
        (s, i, t) -> mapM_ (putStrLn . show) t
    Left err -> putStrLn (show err)

initialSubst = defaultSubst `List.union` fromIdType builtinSubst

traceM msg = trace ('\n' : msg) (return ())

printRunTIResult (s,i,t) = putStrLn $ show t

theta = [("X", TVar "a"), ("Y", TVar "b"), ("Z", TVar "Y")]
eta = [("X", TApp (TVar "f") (TVar "Y")), ("Y", TVar "Z")]
-- theta @@ eta = [("X",f b),("Z",Y)]

tCheck :: Val -> Either String Type
tCheck v = case runTI (tInfer v) initialSubst 0 of
    (s, i, t) -> Right t
    --Fail msg -> Left msg
    --err -> Left (show err)
