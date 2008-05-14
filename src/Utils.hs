module Utils where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set ((\\), union)
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ (
    (<>), (<+>), ($$), ($+$) )
import Debug.Trace (trace)


getKeys = map fst
getVals = map snd

toSet l = Set.fromList l

--elim = Set.toList . Set.fromList
reserved = toSet ["def", "if", "let", "lambda", "filter", "map"]

substitute key val (x:xs) = if x == key
    then
        val : substitute key val xs
    else
        x : substitute key val xs
substitute key val [] = []

allEq l = all (== True) $ zipWith (==) l (tail l)

noDup l = length l == length (List.nub l)

exclude kv k = filter (not . (`elem` k) . fst) kv

subtractMap kv k = foldr Map.delete kv k

{-
idents e = List.nub $ idents' e
    where
        idents' (Ident i) = [i]
        idents' (FunDef _ _ e) = idents' e
        idents' (Lambda _ e) = idents' e
        idents' (List []) = []
        idents' (List l@(x:xs)) = concatMap idents' l
        idents' (Dict l) =  concatMap (\(k, v) -> k : idents' v) l
        idents' (Expr l) = concatMap idents' l
        idents' (App e1 e2) = idents' e1 ++ idents' e2
        idents' x = []
-}

traceM msg = if isDebugSet
    then
        trace ('\n' : msg) (return ())
    else
        return ()
    where
        isDebugSet = True


