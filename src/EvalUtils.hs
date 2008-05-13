module EvalUtils where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\), union)

import Utils
import LangData

unboundVars x = Set.toList (freeVars x \\ reserved)
unboundVarsEnv = freeVarsEnv

freeVars (Ident a) = Set.singleton a

freeVars (Let env body) =
    (freeVarsKeyValList env `union` freeVars body) \\ keys
    where
        keys = toSet $ getKeys env

freeVars (Lambda params body) = freeVars body \\ toSet params

--freeVars (Fun env params body) = freeVarsEnv env `union` freeVars body

{-
freeVars (Fun env params body) = ((freeVarsEnv env `union` freeVars body)
    \\ params') \\ keys
    where
        params' = toSet params
        keys = toSet $ Map.keys env
-}

--freeVars (Closure env body) = -- freeVarsEnv env `List.union`
--    freeVars body \\ Map.keys env
freeVars (FunDef name params body) = freeVars body \\ toSet (name : params)
--freeVars (Prim _ _ []) = []
--freeVars (Prim _ _ x) = concat $ map freeVars x
    -- freeVars x `List.union`
    -- freeVars (Expr xs)
freeVars (Expr (x:xs)) = freeVars x `union` freeVars (Expr xs)
freeVars (Expr []) = Set.empty
freeVars x = Set.empty

freeVarsKeyValList :: KeyValList -> Set.Set Key
freeVarsKeyValList env = vals \\ keys
    where
        vals = Set.unions $ map freeVars (getVals env)
        keys = toSet $ getKeys env

freeVarsEnv env = -- Map.keys env `List.union`
    Set.unions (map freeVars (Map.elems env)) \\ toSet (Map.keys env)


