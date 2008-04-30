{-# LANGUAGE TemplateHaskell #-}

module TCheck where

import qualified Data.List as List
import Data.List ((\\))
import qualified Control.Monad as M
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S
import Debug.Trace (trace)

import LangData
import Parser (parseSingle)
import Type
import TParse
import Trace
import CoreLib
import Utils

type Assumpt = Subst

newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }

instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of
        (s', n', x) -> let TI gx = g x in gx s' n')

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

{-
tCheck :: Subst -> Val -> TI (Subst, Type)
tCheck s (Int _) = return (s, tInt)
tCheck s (Bool _) = return (s, tBool)
tCheck s (Float _) = return (s, tFloat)
tCheck s (Char _) = return (s, tChar)
tCheck s (Str _) = return (s, list tChar)
tCheck s (PrimFun x) = do
    let Just tX = lookup x s
    return (s, tX)

-- TODO: should I check types of each xs?
tCheck s (List (x:xs)) = do
    (s1, tX) <- tCheck s x
    return (s1, list tX)

tCheck s (List []) = do
    v <- newTVar
    return (s, list v)

tCheck s (Ident x) = case lookup x s of
    Just t -> return (s, t)
    otherwise -> error (x ++ " not found in assumptions")
--        t <- newTVar
--        return (s, t)

tCheck s (If pred trueCase falseCase) = do
    (s1, t1) <- tCheck s pred
    s2 <- mgu t1 tBool
    let s2_1 = s2 @@ s1
    (s3, t2) <- tCheck (s2_1 @@ s) trueCase
    let s3_1 = s3 @@ s2_1
    (s4, t3) <- tCheck (s3_1 @@ s) falseCase
    s5 <- mgu (apply s4 t2) t3
    return (s5 @@ s4 @@ s3_1 , apply s5 t3)

tCheck s (Lambda [] e) = tCheck s e

tCheck s (Lambda [p] e) = do
    v <- newTVar
    let pv = (p +-> v)
    let subst = pv @@ s
    (r, t1) <- tCheck subst e -- (replaceIdent (Ident p) (Ident p') e)
    let domain = apply r v
    return (eliminate [p] r, domain `fn` t1)

tCheck s lam@(Lambda _ _) = tCheck s (simplifyLambda lam)

tCheck s (FunDef name args body) = do
    (s1, tF) <- tCheck s (Lambda args body)
    return ((name +-> tF) @@ eliminate args s1, tF)

tCheck s (Let [(k,v)] e) = do
    (s1, tV) <- tCheck s v
    let sNew = (k +-> tV) @@ s1 @@ s
    (s2, tE) <- tCheck sNew e
    return (s2 @@ s1, tE)
-- ty "(let { x = (lambda (x) (+ 1 x)) } (x 1))"

tCheck s (Let kvs e) = tCheck s $ foldr (Let . (:[])) e kvs

tCheck s (Expr [e]) = tCheck s e

tCheck s (Expr l) = tCheck s (foldl1 App l)

tCheck s (App f x) = do
    (sF, tF) <- tCheck s f
    (sX, tX) <- tCheck (sF @@ s) x
    v <- newTVar
    let tF_orig = apply sX tF
    let tF_target = tX `fn` v
    result <- mgu tF_orig tF_target
    return (result @@ sX @@ sF, apply result v)
-}

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

tInfer (Ident x) = do
    s <- getSubst
    case lookup x s of
        Just ts -> toType ts
        otherwise -> fail (x ++ " not found in assumptions")

tInfer (Expr [e]) = tInfer e
tInfer (Expr l) = tInfer (foldl1 App l)

tInfer (App f x) = do
    tF <- tInfer f
    tX <- tInfer x
    s <- getSubst
    v <- newTVar
    let tF_orig = apply s tF
    let tF_target = tX `fn` v
    result <- mgu tF_orig tF_target
    return $ apply result v

tInfer (If pred trueCase falseCase) = do
    tPred <- tInfer pred
    sPred <- mgu tPred tBool
    tTrueCase <- tInfer trueCase
    tFalseCase <- tInfer falseCase
    sFalse <- getSubst
    sTrueFalse <- mgu (apply sFalse tTrueCase) tFalseCase
    return $ apply sTrueFalse tFalseCase



{-
tCheck s (If pred trueCase falseCase) = do
    (s1, t1) <- tCheck s pred
    s2 <- mgu t1 tBool
    let s2_1 = s2 @@ s1
    (s3, t2) <- tCheck (s2_1 @@ s) trueCase
    let s3_1 = s3 @@ s2_1
    (s4, t3) <- tCheck (s3_1 @@ s) falseCase
    s5 <- mgu (apply s4 t2) t3
    return (s5 @@ s4 @@ s3_1 , apply s5 t3)


tCheck s (Lambda [] e) = tCheck s e

tCheck s (Lambda [p] e) = do
    v <- newTVar
    let pv = (p +-> v)
    let subst = pv @@ s
    (r, t1) <- tCheck subst e -- (replaceIdent (Ident p) (Ident p') e)
    let domain = apply r v
    return (eliminate [p] r, domain `fn` t1)

tCheck s lam@(Lambda _ _) = tCheck s (simplifyLambda lam)

tCheck s (FunDef name args body) = do
    (s1, tF) <- tCheck s (Lambda args body)
    return ((name +-> tF) @@ eliminate args s1, tF)

tCheck s (Let [(k,v)] e) = do
    (s1, tV) <- tCheck s v
    let sNew = (k +-> tV) @@ s1 @@ s
    (s2, tE) <- tCheck sNew e
    return (s2 @@ s1, tE)
-- ty "(let { x = (lambda (x) (+ 1 x)) } (x 1))"

tCheck s (Let kvs e) = tCheck s $ foldr (Let . (:[])) e kvs


-}


toType (TScheme [] t) = return t

toType (TScheme (x:xs) t) = do
    v <- newId
    toType $ TScheme xs (replace x v t)

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
    Right v -> case runTI (tInfer v) subst 0 of
        (s,i,t) -> putStrLn $ showSubstType s t
        where
            subst = defaultSubst `List.union` fromIdType builtinSubst


theta = [("X", TVar "a"), ("Y", TVar "b"), ("Z", TVar "Y")]
eta = [("X", TApp (TVar "f") (TVar "Y")), ("Y", TVar "Z")]
-- theta @@ eta = [("X",f b),("Z",Y)]

