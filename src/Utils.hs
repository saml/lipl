module Utils where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.List ((\\))
import LangData

freeVars (Ident a) = [a]
freeVars (Let env body) = freeVars body \\ Map.keys env
freeVars (Lambda params body) = freeVars body \\ params
freeVars (Fun env params body) = freeVars body \\ params
freeVars (Prim _ _ []) = []
freeVars (Prim _ _ (x:xs)) = freeVars x `List.union` freeVars (Expr xs)
freeVars (Expr (x:xs)) = freeVars x `List.union` freeVars (Expr xs)
freeVars (Expr []) = []
freeVars x = []
