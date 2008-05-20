=========
Utils.lhs
=========

.. sc:: haskell

> module Utils where

A module can import other modules using ``import MonduleName``.

.. sc::  haskell

> import qualified Data.Map as Map
> import qualified Data.List as List
> import qualified Data.Set as Set

with ``import qualified ModuleName as Blah``,
all top level functions defined in ModuleName are accessible
through ``Blah.nameOfFunction``. For example,
a function, empty, in ``Data.Map`` can be accessed as ``Map.empty``.
Descriptions of each module (Map, List, ...) can be found in
http://haskell.org/ghc/docs/latest/html/libraries/index.html

.. sc:: haskell

> import Data.Set ((\\), union)

with ``import ModuleName (func1, func2, func3)``,
only func1, func2, and func3 are accessible directly.
Other functions in MouduleName are not accessible from this module.
So, one can call union and ``\\`` on this module.
But other functions defined in ``Data.Set`` module
should be called as ``Set.functionName``.

.. sc:: haskell

> import qualified Text.PrettyPrint.HughesPJ as PP
> import Text.PrettyPrint.HughesPJ (
>     (<>), (<+>), ($$), ($+$) )
> import Debug.Trace (trace)
>
> getKeys = map fst
> getVals = map snd

::

    ghci> let l = [("a", 1), ("b", 1)]
    ghci> getKeys l
    ["a", "b"]
    ghci> getVals l
    [1,1]

.. sc:: haskell

>
> substitute key val (x:xs) = if x == key
>     then
>         val : substitute key val xs
>     else
>         x : substitute key val xs
> substitute key val [] = []
>
> allEq l = all (== True) $ zipWith (==) l (tail l)
>
> noDup l = length l == length (List.nub l)
>
> exclude kv k = filter (not . (`elem` k) . fst) kv
>
> subtractMap kv k = foldr Map.delete kv k
>
>
> traceM msg = if isDebugSet
>     then
>         trace ('\n' : msg) (return ())
>     else
>         return ()
>     where
>         isDebugSet = True
>
> splitAtDot str = reverse $ map reverse $ split str [[]]
>     where
>         split [] acc = acc
>         split ('.':xs) acc = split xs ([] : acc)
>         split (x:xs) (a:as) = split xs ((x : a) : as)
>
> -- | "Foo.bar" ==> ["Foo", "bar"]
> splitOn _ [] = []
> splitOn chr l = f (break (== chr) l)
>     where
>         f (h, []) = [h]
>         f (h, (_:xs)) = h : splitOn chr xs
>
> splitNS ident = let
>     dotIndices = List.findIndices (== '.') ident
>     in
>         if null dotIndices
>             then
>                 ("", ident)
>             else
>                 (\(x,y) -> (x, tail y))
>                     $ splitAt (last dotIndices) ident
>

