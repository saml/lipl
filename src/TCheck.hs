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

{-
instance Eq (m Type) where
    (_ t1) == (_ t2) = t1 == t2
-}

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

--instance (Show a) => Show (Subst -> Int -> (Subst, Int, a)) where
--    show f =

instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of
        (s', n', x) -> let TI gx = g x in gx s' n')


runTI (TI f) = x
    where
        (s, n, x) = f nullSubst 0

getSubst = TI (\s n -> (s, n, s))

unify t1 t2 = do
    s <- getSubst
    u <- mgu (apply s t1) (apply s t2)
    extendSubst u

extendSubst s' = TI (\s n -> (s' @@ s, n, ()))

{-
newTVar k = TI (\s n -> let
    v = TyVar ("v" ++ show n) k
    in
        (s, n + 1, TVar v))
-}

newTVar = TI (\s n -> (s, n+1, TVar ("v" ++ show n)))

testNewTVar = runTI $ sequence [newTVar, newTVar, newTVar]

{-
newtype Enumerator a = Enumerator {
    runEnumerator :: (S.StateT Integer I.Identity) a
    }
-}

{-
newtype Enumerator a = Enumerator a

instance Monad Enumerator where
    return x = Enumerator x
    Enumerator a >>= g = Enumerator
    TI f >>= g = TI (\s n -> case f s n of
        (s', n', x) -> let TI gx = g x in gx s' n')
-}

{-
newId = do
    i <- S.get
    S.put (i+1)
    return ("v" ++ show i)
-}
{-
newtype TC a = TC {
    runTC :: E.ErrorT String (S.StateT (Integer, Subst)
-}

{-
newtype Wrap a = Wrap {
    runWrap :: E.ErrorT Err (S.StateT EnvStack IO) a
} deriving (
    Functor, Monad, F.MonadFix
    , E.MonadError Err, S.MonadState EnvStack, T.MonadIO)
-}

--valToType :: Val -> I.Identity Type
valToType (Int _) = return tInt
valToType (Float _) = return tFloat
valToType (Char _) = return tChar
valToType (Bool _) = return tBool
valToType (Str _) = return $ TApp tList tChar
valToType (List [x]) = do
    t <- valToType x
    return $ TApp tList t
valToType (List l@(x:_)) = if allSameType l
    then
        valToType x >>= (\t -> return $ TApp tList t)
    else
        fail "not homogeneous list"


--find :: Id -> Assumptions -> Either String Type
find x assumptions = case lookup x assumptions of
    Just t -> return t
    otherwise -> fail $ "unbound type variable: " ++ x

{-
--genTVar :: (Monad m, S.MonadState Integer m) =>  s Type
genTVar = do
    i <- S.get
    S.put (i+ (1 :: Integer))
    return $ mkTVar ("v" ++ show i)
-}

type Assumptions = Subst

defaultSubst :: Subst
defaultSubst = [
    ("Int", tInt)
    , ("Float", tFloat)
    , ("Bool", tBool)
    , ("Char", tChar)
    , ("Str", list tChar)
    ]

w :: Subst -> Val -> TI (Subst, Type)
w s (Int _) = return (s, tInt)
w s (Bool _) = return (s, tBool)
w s (Float _) = return (s, tFloat)
w s (Char _) = return (s, tChar)
w s (Str _) = return (s, list tChar)
w s (PrimFun x) = do
    let Just tX = lookup x s
    return (s, $(t 'tX))

{-
w s (List [x]) = do
    (s1, tX) <- w s x
    return (s1, list tX)
-}

-- TODO: should I check types of each xs?
w s (List (x:xs)) = do
    (s1, tX) <- w s x
    return (s1, list tX)

w s (List []) = do
    v <- newTVar
    return (s, list v)

{-
w s (Ident "Int") = return (s, tInt)
w s (Ident "Bool") = return (s, tBool)
w s (Ident "Float") = return (s, tFloat)
w s (Ident "Char") = return (s, tChar)
w s (Ident "Str") = return (s, list tChar)
-}

w s (Ident x) = case lookup x s of
    Just t -> return (s, t)
    otherwise -> error (x ++ " not found in assumptions")
--        t <- newTVar
--        return (s, t)

w s (If pred trueCase falseCase) = do
    (s1, t1) <- w s pred
    s2 <- mgu t1 tBool
    let s2_1 = s2 @@ s1
    (s3, t2) <- w (s2_1 @@ s) trueCase
    let s3_1 = s3 @@ s2_1
    (s4, t3) <- w (s3_1 @@ s) falseCase
    s5 <- mgu (apply s4 t2) t3
    return (s5 @@ s4 @@ s3_1 , apply s5 t3)

w s (Lambda [] e) = w s e

w s (Lambda [p] e) = do
    v <- newTVar
    let subst = s @@ (p +-> v)
    (r, t1) <- w subst e
    let domain = apply r v
    return (r, domain `fn` t1)

w s lam@(Lambda _ _) = w s (simplifyLambda lam)

w s (FunDef name args body) = do
    (s1, tF) <- w s (Lambda args body)
    return ((name +-> tF) @@ s1, tF)

{-
w s (Expr (f:g:xs)) = do
    (s1, t1) <- w s f
    (s2, t2) <- w (s1 @@ s) g
    v <- newTVar
    u <- mgu (apply s2 t1) (t2 `fn` v)
    return (u @@ s1 @@ s, apply u v)
-}



w s (Expr [e]) = w s e

w s (Expr l) = w s (foldl1 App l)


{-
w s (Expr [f, x]) = do
    (s1, tF) <- w s f
    (s2, tX) <- w (s1 @@ s) x
    v <- newTVar
    result <- mgu (apply s2 tF) (tX `fn` v)
    return (result @@ s2 @@ s1, apply result v)

w s (Expr (f:x:xs)) = do
    (s1, tF) <- w s f
    (s2, tX) <- w (s1 @@ s) x
    v <- newTVar
    let tF_orig = apply s2 tF
    let tF_target = tX `fn` v
    tF_result <- mgu tF_orig tF_target
    (s3, tXs) <- w (tF_result @@ s2 @@ s1 @@ s) (Expr xs)
    return (s3, tXs)
-}

w s (App f x) = do
    (sF, tF) <- w s f
    (sX, tX) <- w (sF @@ s) x
    v <- newTVar
    let tF_orig = apply sX tF
    let tF_target = tX `fn` v
    result <- mgu tF_orig tF_target
    return (result @@ sX @@ sF, apply result v)

--listToApp = foldl1 App

simplifyLambda lam@(Lambda [] e) = lam
simplifyLambda lam@(Lambda [x] e) = lam
simplifyLambda lam@(Lambda (x:xs) e) =
    Lambda [x] (simplifyLambda (Lambda xs e))
simplifyLambda (Expr [x]) = simplifyLambda x

allSameType l = and $
    zipWith (\a b -> I.runIdentity (valToType a)
        == I.runIdentity (valToType b)) l (tail l)

{-
t s = case parseSingle s of
    Right v -> I.runIdentity $ valToType v
    otherwise -> error "ill-typed"
-}

ty s = case parseSingle s of
    Right v -> putStrLn
        $ showSubstTypePair $ runTI
        $ w (defaultSubst `List.union` builtinSubst) v
    otherwise -> error "parse error"
