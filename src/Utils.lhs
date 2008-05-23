=========
Utils.lhs
=========

.. sectnum::
.. contents::

.. sc:: lhs

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

.. sc:: lhs

> import Data.Set ((\\), union)

with ``import ModuleName (func1, func2, func3)``,
only func1, func2, and func3 are accessible directly.
Other functions in MouduleName are not accessible from this module.
So, one can call union and ``\\`` on this module
(note the parenthesis around ``\\``).
But other functions defined in ``Data.Set`` module
should be called as ``Set.functionName`` (because Data.Set
module was qualified as Set).

.. sc:: lhs

> import qualified Text.PrettyPrint.HughesPJ as PP
> import Text.PrettyPrint.HughesPJ (
>     (<>), (<+>), ($$), ($+$) )
> import Debug.Trace (trace)
>
> getKeys = map fst
> getVals = map snd

::

    ghci> :l Utils
    ghci> let l = [("a", 1), ("b", 1)]
    ghci> getKeys l
    ["a", "b"]
    ghci> getVals l
    [1,1]

.. sc:: lhs

> allEqWith eq l = all (== True) $ zipWith eq l (tail l)

allEqWith tests if the list l has all same elements, where equality
is defined by function eq::

    l:                  [e1,e2,...,eN]
    tail l:             [e2,e3,...]
    zipWith eq ...  :   [e1 `eq` e2, e2 `eq` e3, ..., eN-1 `eq` eN]
    all (== True) ... : \-    are they all True ??               -/

.. sc:: lhs

> allEq :: (Eq a) => [a] -> Bool
> allEq = allEqWith (==)

allEq tests if the list l has all same elements where equality
is checked by ``==``.


.. sc:: lhs

> noDup l = length l == length (List.nub l)

noDup tests if l has no duplicate::

    l: [e1, e2, ..., eN]
    length l: N
    List.nub l: [e1, e2, ..., eM] where all duplicates are removed.

So, when length of l is same as length of ``List.nub l``, l did not have
duplicates to begin with.

.. sc:: lhs

> subtractMap kv k = foldr Map.delete kv k

subtractMap takes a Map and a list of keys. And it deletes
items from the Map that are mapped to keys::

    let m = Map.fromList [("a",1), ("b",2)]

    foldr Map.delete m ["c", "b", "a"]
    ==> Map.delete "c" (Map.delete "b" (Map.delete "a" m))
    ==> Map.delete "c" (Map.delete "b" m')
        where m' is Map.fromList [("b",2)], ("a",1) is deleted.
    ==> Map.delete "c" m''
        where m'' is Map.empty, ("b",2) is deleted.
    ==> Map.empty

.. sc:: lhs

> traceM msg = if isDebugSet
>     then
>         trace msg (return ())
>     else
>         return ()
>     where
>         isDebugSet = True

traceM can be used in a monad to print out msg::

    do
        traceM "hello"
        ...

.. sc:: lhs

> -- | "Foo.bar" ==> ["Foo", "bar"]
> splitOn _ [] = []
> splitOn chr l = f (break (== chr) l)
>     where
>         f (h, []) = [h]
>         f (h, (_:xs)) = h : splitOn chr xs
>

splitOn splits a list (including string) on an element (character)::

    ghci> :l Utils
    ghci> splitOn '.' "Utils.lhs"
    ["Utils", "lhs"]

