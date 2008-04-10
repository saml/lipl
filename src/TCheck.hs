module TCheck where

import qualified Data.List as List
import Data.List ((\\))
import qualified Control.Monad as M

type Id = String

data Kind = Star | KFun Kind Kind
    deriving (Show, Eq)


data Type = TVar TyVar
    | TConst TyConst
    | TApp Type Type
    deriving (Show, Eq)

data TyVar = TyVar Id Kind
    deriving (Show, Eq)

data TyConst = TyConst Id Kind
    deriving (Show, Eq)

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
