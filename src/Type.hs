module Type where

import qualified Data.List as List

type Id = String

{-
data Kind = Star | KFun Kind Kind
    deriving (Show, Eq)

data Type = TVar TyVar
    | TConst TyConst
    | TApp Type Type
    deriving (Eq)
-}

data Type = TVar Id
    | TConst Id
    | TApp Type Type
    deriving (Eq)

instance Show Type where
    show = ppType

ppType (TVar v) = v
ppType (TConst c) = c
ppType (TApp (TConst "[]") a) = "[" ++ ppType a ++ "]"
ppType (TApp (TConst "->") a) = ppType a ++ " ->"
ppType (TApp a b) = ppType a ++ " " ++ ppType b

{-
ppType (TVar v) = show v
ppType (TConst c) = show c
ppType (TApp (TConst (TyConst "[]" _)) a) = "[" ++ show a ++ "]"
ppType (TApp (TConst (TyConst "(->)" _)) a) = show a ++ " -> "
ppType (TApp a b) = show a ++ " " ++ show b
-}

{-
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
-}

{-
tUnit = TConst $ TyConst "()" Star
tChar = TConst $ TyConst "Char" Star
tInt = TConst $ TyConst "Int" Star
tFloat = TConst $ TyConst "Float" Star
tBool = TConst $ TyConst "Bool" Star
tList = TConst $ TyConst "[]" Star
tArrow = TConst $ TyConst "(->)" Star
tTuple2 = TConst $ TyConst "(,)" Star
--tList = TConst $ TyConst "[]" (KFun Star Star)
--tArrow = TConst $ TyConst "(->)" (KFun Star (KFun Star Star))
--tTuple2 = TConst $ TyConst "(,)" (KFun Star (KFun Star Star))
-}


tUnit = TConst "()"
tChar = TConst "Char"
tInt = TConst "Int"
tFloat = TConst "Float"
tBool = TConst "Bool"
tList = TConst "[]"
tArrow = TConst "->"
tTuple2 = TConst "(,)"

infix 4 `fn`
a `fn` b = TApp (TApp tArrow a) b

list = TApp tList

pair a b = TApp (TApp tTuple2 a) b

mkTVar = TVar
--mkTVar s = TVar $ mkTyVar s

-- mkTyVar s = TyVar s Star

{-
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
-}

type Subst = [(Id, Type)]

nullSubst = []

(+->) :: Id -> Type -> Subst
v +-> t = [(v,t)]

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [Id]

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

--testMgu = I.runIdentity $ mgu (mkTVar "a" `fn` tInt) (tChar `fn` mkTVar "b")

varBind u t
    | t == TVar u = return nullSubst
    | u `elem` tv t = fail "occurs check fails"
    -- | kind u /= kind t = fail "kinds do not match"
    | otherwise = return (u +-> t)

match (TApp f1 a1) (TApp f2 a2) = do
    sf <- match f1 f2
    sa <- match a1 a2
    merge sf sa
match (TVar u) t = return (u +-> t)
match (TConst c1) (TConst c2)
    | c1 == c2 = return nullSubst
match _ _ = fail "types do not match"

