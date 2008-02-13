-- dependently typed
-- http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html

module Main where

import Data.List (union, (\\))

-- Untyped Lambda Calculus

type Sym = String

data Expr = Var Sym
    | App Expr Expr
    | Lam Sym Expr
    deriving (Eq, Read, Show)

-- (λx.λy.x)(λz.z)
-- (λy.λz.z)

test = App (Lam "x" $ Lam "y" $ Var "x") (Lam "z" $ Var "z")

-- Weak Head Normal Form (WHNF)

whnf :: Expr -> Expr
whnf ee = spine ee [] where
    spine (App f a) as = spine f (a:as)
    spine (Lam s e) (a:as) = spine (subst s a e) as
    spine f as = foldl App f as

freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i e) = freeVars e \\ [i]

subst :: Sym -> Expr -> Expr -> Expr
subst v x b = sub b where
    sub e@(Var i) = if i == v then x else e
    sub (App f a) = App (sub f) (sub a)
    sub (Lam i e) = if v == i
        then Lam i e
        else if i `elem` fvx then
            let i' = cloneSym e i
                e' = substVar i i' e
            in Lam i' (sub e')
        else
            Lam i (sub e)
    fvx = freeVars x
    cloneSym e i = loop i where
        loop i' = if i' `elem` vars
            then loop (i ++ "'")
            else i'
        vars = fvx ++ freeVars e

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

