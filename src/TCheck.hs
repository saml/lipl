module TCheck where

import qualified Data.List as List
import Data.List ((\\))
import qualified Control.Monad as M
import qualified Control.Monad.Identity as I
import qualified Control.Monad.Error as E
import qualified Control.Monad.State as S

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
tBool = TConst $ TyConst "Bool" Star
tList = TConst $ TyConst "[]" Star
tArrow = TConst $ TyConst "(->)" Star
tTuple2 = TConst $ TyConst "(,)" Star
--tList = TConst $ TyConst "[]" (KFun Star Star)
--tArrow = TConst $ TyConst "(->)" (KFun Star (KFun Star Star))
--tTuple2 = TConst $ TyConst "(,)" (KFun Star (KFun Star Star))

infix 4 `fn`
a `fn` b = TApp (TApp tArrow a) b

list = TApp tList

pair a b = TApp (TApp tTuple2 a) b

mkTVar s = TVar $ mkTyVar s

mkTyVar s = TyVar s Star

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

testMgu = I.runIdentity $ mgu (mkTVar "a" `fn` tInt) (tChar `fn` mkTVar "b")

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
    (mkTyVar "+", tInt `fn` tInt)
    ]

w :: Subst -> Val -> TI (Subst, Type)
w s (Int _) = return (s, tInt)
w s (Bool _) = return (s, tBool)
--w s (Ident "+") = return (s, tInt `fn` tInt)
w s (PrimFun x) = do
    let Just t = lookup (mkTyVar x) s
    return (s, t)

w s (Ident x) = case lookup (mkTyVar x) s of
    Just t -> return (nullSubst, t)
    otherwise -> do
        t <- newTVar Star
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
    v <- newTVar Star
    (r, t) <- w (s ++ (mkTyVar p +-> v)) e
    return (r, apply r v `fn` t)

w s (Expr (f:g:xs)) = do
    (s1, t1) <- w s f
    (s2, t2) <- w (s1 @@ s) g
    v <- newTVar Star
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
