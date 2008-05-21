=============
EvalUtils.lhs
=============

EvaluUtils module has functions used in Evaluator.

.. sc:: haskell

> module EvalUtils where
>
> import qualified Data.Map as Map
> import qualified Data.Set as Set
> import Data.Set ((\\), union)
>
> import Utils
> import LangData
> import Parser
>
> toSet l = Set.fromList l

toSet converts a list to Set.

.. sc:: haskell

> unboundVars x = Set.toList (freeVars x)

unboundVars returns list of Key's that are not bound (free variables)::

    ghci> :l EvalUtils
    ghci> unboundVars (Ident "a")
    ["a"]
    ghci> unboundVars (Lambda ["a"] (Expr [Ident "a", Ident "b"]))
    ["b"]

"a" is free in the first expression.
However, it is bound in the lambda expression. So, only "b" is returned.

.. sc:: haskell

> freeVars (Ident a) = Set.singleton a
>
> freeVars (Let env body) =
>     (freeVarsKeyValList env `union` freeVars body) \\ keys
>     where
>         keys = toSet $ getKeys env
>
> freeVars (Lambda params body) = freeVars body \\ toSet params
>
> freeVars (FunDef name params body) = freeVars body \\ toSet (name : params)
>
> freeVars (Expr []) = Set.empty
> freeVars (Expr [x]) = freeVars x
> freeVars (Expr l) = Set.unions (map freeVars l)
>
> freeVars (At _ x) = freeVars x
> freeVars x = Set.empty
>
> freeVarsKeyValList :: KeyValList -> Set.Set Key
> freeVarsKeyValList env = vals \\ keys
>     where
>         vals = Set.unions $ map freeVars (getVals env)
>         keys = toSet $ getKeys env

freeVars returns a Set of Key's that are free in a given Val.
It is defined case by case using pattern matching.
For example, when the given Val is ``Ident a``, then a singleton Set of
a is returned.
For other cases, Set.union and Set difference operation (``\\``) are used
to get all free variables.


