=============
LangUtils.lhs
=============

.. sectnum::
.. contents::

LangUtils module has useful functions
related to LIPL Val.

.. sc:: lhs

> module LangUtils where
>
> import qualified Data.Set as Set
> import Data.Set ((\\), union)

With ``import ModuleName (func1, func2, func3)``,
only func1, func2, and func3 are accessible directly
(``(func1, func2, ...)`` is called an import list).
Other functions in MouduleName are not accessible from this module.
So, one can call union and ``\\`` on this module
(note the parenthesis around ``\\`` in the import list).
But other functions defined in ``Data.Set`` module
should be called as ``Set.functionName`` (because Data.Set
module was qualified as Set).

.. sc:: lhs

> import Utils
> import LangData
> import Parser
>
> toSet l = Set.fromList l

toSet converts a list to Set.

.. sc:: lhs

> unboundVars x = Set.toList (freeVars x)

unboundVars returns a list of Keys that are not bound (free variables)::

    ghci> :l LangUtils
    ghci> unboundVars (Ident "a")
    ["a"]
    ghci> unboundVars (Lambda ["a"] (Expr [Ident "a", Ident "b"]))
    ["b"]

"a" is free in the first expression.
However, it is bound in the lambda expression. So, only "b" is returned.

.. sc:: lhs

> freeVars (Ident a) = Set.singleton a
>
> freeVars (Let env body) =
>     (freeVarsKeyValList env `union` freeVars body) \\ keys
>     where
>         keys = toSet $ getKeys env
>
> freeVars (Lambda params body) = freeVars body \\ toSet params
>
> freeVars (FunDef name params body) = freeVars body
>     \\ toSet (name : params)
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

freeVars returns a Set of Keys that are free in a given Val.
It is defined case by case using pattern matching.
For example, when the given Val is ``Ident a``, then a singleton Set of
a is returned.
For other cases, Set.union and Set difference operation (``\\``) are used
to get all free variables.

.. sc:: lhs

> simplifyLambda lam@(Lambda [] e) = lam
> simplifyLambda lam@(Lambda [x] e) = lam
> simplifyLambda lam@(Lambda (x:xs) e) =
>     Lambda [x] (simplifyLambda (Lambda xs e))
> simplifyLambda (Expr [x]) = simplifyLambda x

simplifyLambda converts a lambda with multiple parameters
to a normal lambda that takes only 1 parameter::

    (lambda (x1 x2 ... xN) e)
    ==> (lambda (x1) (lambda (x2) ... (lambda (xN) e)))

