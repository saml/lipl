module TCheck where

import qualified Data.List as List
import Data.List ((\\))
import qualified Control.Monad as M
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S

import LangData
import Parser
import Type

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
    ("+", tInt `fn` tInt)
    ]

w :: Subst -> Val -> TI (Subst, Type)
w s (Int _) = return (s, tInt)
w s (Bool _) = return (s, tBool)
--w s (Ident "+") = return (s, tInt `fn` tInt)
w s (PrimFun x) = do
    let Just t = lookup x s
    return (s, t)

w s (Ident x) = case lookup x s of
    Just t -> return (nullSubst, t)
    otherwise -> do
        t <- newTVar
        return (nullSubst, t)

w s (If pred trueCase falseCase) = do
    (s1, t1) <- w s pred
    s2 <- mgu t1 tBool
    let s2_1 = s2 @@ s1
    (s3, t2) <- w (s2_1 @@ s) trueCase
    let s3_1 = s3 @@ s2_1
    (s4, t3) <- w (s3_1 @@ s) falseCase
    s5 <- mgu (apply s4 t2) t3
    return (s5 @@ s4 @@ s3_1 , apply s5 t3)

w s (Lambda [p] e) = do
    v <- newTVar
    (r, t) <- w (s ++ (p +-> v)) e
    return (r, apply r v `fn` t)

w s (Expr (f:g:xs)) = do
    (s1, t1) <- w s f
    (s2, t2) <- w (s1 @@ s) g
    v <- newTVar
    u <- mgu (apply s2 t1) (t2 `fn` v)
    return (u @@ s1 @@ s, apply u v)

w s (Expr [e]) = w s e

{-
w :: Assumptions -> Val -> TI Type
w a (Ident x) = do
    t <- case lookup x a of
        Just t -> return t
        otherwise -> newTVar Star
    --t <- (find x assumpt `E.catchError` (\e -> newTVar Star))
    return t

w a (If p e1 e2) = if w a p == tBool
    then

    else
        fail "type of predicate is not Bool"
-}

allSameType l = and $
    zipWith (\a b -> I.runIdentity (valToType a)
        == I.runIdentity (valToType b)) l (tail l)

t s = case parseSingle s of
    Right v -> I.runIdentity $ valToType v
    otherwise -> error "ill-typed"

ty s = case parseSingle s of
    Right v -> runTI $ w defaultSubst v
    otherwise -> error "parse error"
