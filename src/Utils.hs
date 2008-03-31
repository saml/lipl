module Utils (freeVars) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.List ((\\))
import LangData

elim = Set.toList . Set.fromList

freeVars (Ident a) = [a]
freeVars (Let env body) =  elim (freeVarsEnv env `List.union`
    (freeVars body \\ Map.keys env))
freeVars (Lambda params body) = elim (freeVars body \\ params)
freeVars (Fun env params body) =elim (freeVarsEnv env `List.union`
    (freeVars body \\ params))
freeVars (Closure env body) = elim (freeVarsEnv env `List.union`
    freeVars body)
freeVars (FunDef name params body) = elim (freeVars body \\
    (name : params))
freeVars (Prim _ _ []) = []
freeVars (Prim _ _ (x:xs)) = elim (freeVars x `List.union`
    freeVars (Expr xs))
freeVars (Expr (x:xs)) = elim (freeVars x `List.union` freeVars (Expr xs))
freeVars (Expr []) = []
freeVars x = []

freeVarsEnv env = concat $ map freeVars (Map.elems env)



test = freeVars (Let
    (Map.fromList [("x", Expr [PrimFun "head", Ident "l"])])
    (Expr [Ident "x"]))
