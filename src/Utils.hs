module Utils (freeVars) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.List ((\\))
import LangData

freeVars (Ident a) = [a]
freeVars (Let env body) =  freeVarsEnv env `List.union`
    (freeVars body \\ Map.keys env)
freeVars (Lambda params body) = freeVars body \\ params
freeVars (Fun env params body) = freeVarsEnv env `List.union`
    (freeVars body \\ params)
freeVars (Closure env body) = freeVarsEnv env `List.union`
    freeVars body
freeVars (FunDef name params body) = freeVars body \\ (name : params)
freeVars (Prim _ _ []) = []
freeVars (Prim _ _ (x:xs)) = freeVars x `List.union` freeVars (Expr xs)
freeVars (Expr (x:xs)) = freeVars x `List.union` freeVars (Expr xs)
freeVars (Expr []) = []
freeVars x = []

freeVarsEnv env = concat $ map freeVars (Map.elems env)



test = freeVars (Let
    (Map.fromList [("x", Expr [PrimFun "head", Ident "l"])])
    (Expr [Ident "x"]))
