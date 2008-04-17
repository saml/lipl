module Utils where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set ((\\), union)
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<>), (<+>), ($$), ($+$) )
import Debug.Trace (trace)

import LangData

getKeys = map fst
getVals = map snd

toSet l = Set.fromList l

--elim = Set.toList . Set.fromList
reserved = toSet ["def", "if", "let", "lambda", "filter", "map"]

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

substitute key val (x:xs) = if x == key
    then
        val : substitute key val xs
    else
        x : substitute key val xs
substitute key val [] = []

{-
ppKeyVal ppk ppv (k, v) = ppk k
    <+> PP.text "="
    <+> ppv v

ppKeyValList :: [(k,v)] -> PP.Doc
ppKeyValList l = ppDict $ map ((PP.empty $$) . ppKeyVal) l

ppDict l = PP.braces $ PP.fsep $ PP.punctuate PP.comma l
-}

