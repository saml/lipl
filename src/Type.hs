module Type where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<>), (<+>), ($$), ($+$) )

import Utils

type Id = String

data Type = TVar { getId :: Id }
    | TConst { getId :: Id }
    | TApp Type Type
    deriving (Eq)

instance Show Type where
    show = PP.render . ppType


fromTVar (TVar v) = v

ppType (TVar v) = PP.text v
ppType (TConst c) = PP.text c
ppType (TApp (TConst "[]") a) = PP.brackets (ppType a)
ppType (TApp (TApp (TConst "->") a) b) =
    PP.parens $ PP.fsep [ppType a, PP.text "->", ppType b]
ppType (TApp a b) = ppType a <+> ppType b

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

type Subst = [(Id, TScheme)]

ppIdType (i,t) = PP.fsep [PP.text i, PP.text "::", ppType t]

ppIdTypeList l = map ((PP.empty $$) . ppIdType) l

ppSubst l = PP.braces $ PP.fsep $ PP.punctuate PP.comma (ppIdTypeList l)

showSubst = PP.render . ppSubst

showSubstTypePair (s,t) = PP.render (ppSubst s $$ ppType t)

nullSubst = []

(+->) :: Id -> TScheme -> Subst
v +-> ts = [(v, ts)]

-- type abstraction
-- ((TScheme [x,y] ([y] -> x)) Int Char) ==> [Char] -> Int
data TScheme = TScheme [Id] Type
    deriving (Show, Eq)

replace a b e@(TVar v)
    | a == v = TVar b
    | otherwise = e
replace a b e@(TConst _) = e
replace a b (TApp t1 t2) = TApp (replace a b t1) (replace a b t2)

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [Id]

instance Types Type where
    apply s v@(TVar u) = case lookup u s of
        Just (TScheme _ t) -> t
        Nothing -> v
    apply s (TApp f a) = TApp (apply s f) (apply s a)
    apply s t = t

    tv (TVar u) = [u]
    tv (TApp f a) = tv f `List.union` tv a
    tv _ = []

instance (Types a) => Types [a] where
    apply s = map (apply s)
    tv = List.nub . concat . map tv

instance Types TScheme where
    apply s (TScheme l e) = TScheme l $ apply (s `exclude` l) e
    tv (TScheme l e) = tv e

infixr 4 @@
s1 @@ s2 = (Map.toList . Map.fromListWith (\x y -> y))
    ([(u, apply s1 t) | (u, t) <- s2] ++ s1)

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
-- mgu (list tChar `fn` TVar "b") (TVar "a" `fn` (TVar "a" `fn` tChar))

varBind u t
    | t == TVar u = return nullSubst
    | u `elem` tv t = fail "occurs check fails"
    | otherwise = return (u +-> TScheme [] t)

match (TApp f1 a1) (TApp f2 a2) = do
    sf <- match f1 f2
    sa <- match a1 a2
    merge sf sa
match (TVar u) t = return (u +-> TScheme [] t)
match (TConst c1) (TConst c2)
    | c1 == c2 = return nullSubst
match _ _ = fail "types do not match"

