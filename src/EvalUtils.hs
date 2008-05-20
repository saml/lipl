module EvalUtils where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\), union)

import Utils
import LangData
import Parser

toSet l = Set.fromList l

--reserved = toSet ["def", "if", "let", "lambda"]

unboundVars x = Set.toList (freeVars x) -- \\ reserved)

freeVars (Ident a) = Set.singleton a

freeVars (Let env body) =
    (freeVarsKeyValList env `union` freeVars body) \\ keys
    where
        keys = toSet $ getKeys env

freeVars (Lambda params body) = freeVars body \\ toSet params

freeVars (FunDef name params body) = freeVars body \\ toSet (name : params)

freeVars (Expr []) = Set.empty
freeVars (Expr [x]) = freeVars x
freeVars (Expr l) = Set.unions (map freeVars l)
-- freeVars (Expr (x:xs)) = freeVars x `union` freeVars (Expr xs)

freeVars (At _ x) = freeVars x
freeVars x = Set.empty

freeVarsKeyValList :: KeyValList -> Set.Set Key
freeVarsKeyValList env = vals \\ keys
    where
        vals = Set.unions $ map freeVars (getVals env)
        keys = toSet $ getKeys env

-- freeVarsEnv env = -- Map.keys env `List.union`
--    Set.unions (map freeVars (Map.elems env)) \\ toSet (Map.keys env)


