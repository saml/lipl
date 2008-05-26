=========
Utils.lhs
=========

.. sectnum::
.. contents::

Utils module defines useful functions used by many other modules.

.. sc:: lhs

> module Utils where

A module can import other modules using ``import MonduleName``.

.. sc::  haskell

> import qualified Data.List as List
> import qualified Data.Map as Map

with ``import qualified ModuleName as Blah``,
all top level functions defined in ModuleName are accessible
through ``Blah.nameOfFunction``. For example,
a function, empty, in ``Data.Map`` can be accessed as ``Map.empty``.
Descriptions of each module (Map, List, ...) can be found in
http://haskell.org/ghc/docs/latest/html/libraries/index.html

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
    ghci> let l = [("a", 2), ("b", 1)]
    ghci> getKeys l
    ["a", "b"]
    ghci> getVals l
    [2,1]

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
        where m' is Map.fromList [("b",2)],
        for ("a",1) is deleted by Map.delete "a" m.
    ==> Map.delete "c" m''
        where m'' is Map.empty,
        for ("b",2) is deleted by Map.delete "b" m'.
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
        ...
        traceM "hello"
        ...

    ==> prints hello to screen

``do`` is Haskell keyword that starts a do block.
do block (or do notation) is related to monads, which
will be discussed later.


.. sc:: lhs

> -- | "Foo.bar" ==> ["Foo", "bar"]
> splitOn _ [] = []
> splitOn chr l = f (break (== chr) l)
>     where
>         f (h, []) = [h]
>         f (h, (_:xs)) = h : splitOn chr xs
>

splitOn splits a list (including string) on an element (character)::

    splitOn '.' "www.example.com"
    ==> f (break (== '.') "www.example.com")
    ==> f ("www", ".example.com")
    ==> "www" : splitOn '.' "example.com"
    ==> "www" : splitOn '.' "example.com"
    ==> "www" : f (break (== '.') "example.com")
    ==> "www" : f ("example", ".com")
    ==> "www" : "example" : splitOn '.' "com"
    ==> "www" : "example" : f (break (== '.') "com")
    ==> "www" : "example" : f ("com", [])
    ==> "www" : "example" : ["com"]
    ==> ["www", "example", "com"]

    ghci> :l Utils
    ghci> splitOn '.' "Utils.lhs"
    ["Utils", "lhs"]

Layout
======

Haskell supports layout so that source code is not cluttered
with ``{ }``, ``;``, ...etc.
For example, above splitOn function could have been written as::

    splitOn _ [] = []
    splitOn chr l = f (break (== chr) l) where {
        f (h, []) = [h];
     f (h, (_:xs)) = h : splitOn chr xs;
    }

Note that two definitions of function f are not aligned.
From now on layout will be used sometimes without explicit ``{ }``, ``;``, ...
So, one would need to align source code properly
when copying source code written here.


