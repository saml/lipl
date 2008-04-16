module TCheck where

import qualified Data.List as List
import Data.List ((\\))
import qualified Control.Monad as M
import qualified Control.Monad.Identity as I

import LangData
import Parser

type Id = String

data Kind = Star | KFun Kind Kind
    deriving (Show, Eq)

data Type = TVar TyVar
    | TConst TyConst
    | TApp Type Type
    deriving (Eq)

instance Show Type where
    show = ppType

ppType (TVar v) = show v
ppType (TConst c) = show c
ppType (TApp (TConst (TyConst "[]" _)) a) = "[" ++ show a ++ "]"
ppType (TApp (TConst (TyConst "(->)" _)) a) = show a ++ " -> "
ppType (TApp a b) = show a ++ " " ++ show b

data TyVar = TyVar Id Kind
    deriving (Eq)

instance Show TyVar where
    show = ppTyVar

ppTyVar (TyVar v _) = v

data TyConst = TyConst Id Kind
    deriving (Eq)

instance Show TyConst where
    show = ppTyConst

--ppTyConst (TyConst "(->)" _) = "->"
ppTyConst (TyConst c _) = c


tUnit = TConst $ TyConst "()" Star
tChar = TConst $ TyConst "Char" Star
tInt = TConst $ TyConst "Int" Star
tFloat = TConst $ TyConst "Float" Star
tList = TConst $ TyConst "[]" (KFun Star Star)
tArrow = TConst $ TyConst "(->)" (KFun Star (KFun Star Star))
tTuple2 = TConst $ TyConst "(,)" (KFun Star (KFun Star Star))

infix 4 `fn`
a `fn` b = TApp (TApp tArrow a) b

list = TApp tList

pair a b = TApp (TApp tTuple2 a) b

tyvar s = TVar (TyVar s Star)

class HasKind t where
    kind :: t -> Kind

instance HasKind TyVar where
    kind (TyVar _ k) = k

instance HasKind TyConst where
    kind (TyConst _ k) = k

instance HasKind Type where
    kind (TConst c) = kind c
    kind (TVar v) = kind v
    kind (TApp f arg) = case kind f of
        KFun param result | param == (kind arg) -> result

type Subst = [(TyVar, Type)]

nullSubst = []

v +-> t | kind v == kind t = [(v,t)]

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [TyVar]

instance Types Type where
    apply s v@(TVar u) = case lookup u s of
        Just t -> t
        Nothing -> v
    apply s (TApp f a) = TApp (apply s f) (apply s a)
    apply s t = t

    tv (TVar u) = [u]
    tv (TApp f a) = tv f `List.union` tv a
    tv _ = []

instance (Types a) => Types [a] where
    apply s = map (apply s)
    tv = List.nub . concat . map tv

infixr 4 @@
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge s1 s2 = if agree then return (s1 ++ s2) else fail "merge fails"
    where
        agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                    (map fst s1 `List.intersect` map fst s2)

mgu (TApp f1 a1) (TApp f2 a2) = do
    s1 <- mgu f1 f2
    s2 <- mgu (apply s1 a1) (apply s1 a2)
    return (s2 @@ s1)
mgu (TVar v) t = varBind v t
mgu t (TVar v) = varBind v t
mgu (TConst c1) (TConst c2)
    | c1 == c2 = return nullSubst
mgu _ _ = fail "types do not unify"

varBind u t
    | t == TVar u = return nullSubst
    | u `elem` tv t = fail "occurs check fails"
    | kind u /= kind t = fail "kinds do not match"
    | otherwise = return (u +-> t)

match (TApp f1 a1) (TApp f2 a2) = do
    sf <- match f1 f2
    sa <- match a1 a2
    merge sf sa
match (TVar u) t
    | kind u == kind t = return (u +-> t)
match (TConst c1) (TConst c2)
    | c1 == c2 = return nullSubst
match _ _ = fail "types do not match"

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

newTVar k = TI (\s n -> let
    v = TyVar ("v" ++ show n) k
    in
        (s, n + 1, TVar v))

testNewTVar = runTI $ sequence [newTVar Star, newTVar Star, newTVar Star]

valToType :: Val -> I.Identity Type
valToType (Int _) = return tInt
valToType (Float _) = return tFloat
valToType (Char _) = return tChar
valToType (Str _) = return $ TApp tList tChar
valToType (List [x]) = do
    t <- valToType x
    return $ TApp tList t
valToType (List l@(x:_)) = if allSameType l
    then
        valToType x >>= (\t -> return $ TApp tList t)
    else
        fail "not homogeneous list"

allSameType l = and $
    zipWith (\a b -> I.runIdentity (valToType a)
        == I.runIdentity (valToType b)) l (tail l)


t s = case parseSingle s of
    Right v -> I.runIdentity $ valToType v
    otherwise -> error "ill-typed"


