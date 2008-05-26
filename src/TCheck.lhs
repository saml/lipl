==========
TCheck.lhs
==========

TCheck module defines tInfer action that does type inference
using unification.

.. sc:: lhs

> {-# LANGUAGE FlexibleContexts #-}
>
> module TCheck where
>
> import qualified Data.Map as Map
> import Data.List ((\\))
> import qualified Control.Monad as M
> import qualified Control.Monad.Error as E
>
> import LangData
> import LangUtils
> import Type
> import CoreLib (builtinSubst)
> import Utils
> import TIMonad
> import PosMonad
> import Error
>
> tInfer :: (MonadTI m, MonadPos m, E.MonadError Err m) => Val -> m Type
> tInfer (At pos e) = do
>     setSourcePos pos
>     tInfer e

For Val value constructed with At data constructor,
set current SourcePos to the given pos and type infer e.
When something fails later on,
the saved SourcePos can be retrieved and be reported.

.. sc:: lhs

> tInfer (Int _) = return tInt
> tInfer (Bool _) = return tBool
> tInfer (Float _) = return tFloat
> tInfer (Char _) = return tChar
> tInfer (Str _) = return $ list tChar

For above literals (Int, Bool, ...) just return
corresponding built-in types (Int, Bool, ...).

.. sc:: lhs

> tInfer (PrimFun x) = do
>     s <- getSubst
>     case Map.lookup x s of
>         Just ts -> do
>             t <- toType ts
>             return t
>         otherwise -> throwErr ("primitive function not found: " ++ x)

For built-in functions (PrimFun),
find the type of it in the current substitution (type variable
and type mapping).
When it's not found, throwErr (it throws error with
current SourcePos).
When it's found, instantiate the bound type scheme using toType.
For example, built-in function ``fst`` has type ``(a, b) -> a``
where a and b are universally quantified (``forall a b. (a, b) -> a``).
So, toType will update universally quantified type variables
in the type of fst, a and b,
with fresh variables.
Otherwise, there can be errors when many functions use the same type
variable a and b. For example, once a is bound to Int because of
``fst (1,"hello")``, inferring type of ``head "123"`` can fail because
type of head, ``[a] -> a`` uses the same type variable, a.

.. sc:: lhs

> tInfer (List []) = do
>     v <- newTVar
>     return $ list v

Type of an empty list is ``[a]`` where a is a fresh type variable.

.. sc:: lhs

> tInfer (List l) = do
>     ts <- mapM tInfer l
>     if allEqWith tEq ts
>         then
>             return (list (head ts))
>         else
>             throwErr ("not homogeneous list: " ++ show (List l))

To check homogeneousity of a list l,
tInfer is called on every element in l. Then, allEqWith
checks if all elements' types are the same.
Since tEq normalizes type variables before checking equality,
``[(lambda (x) x), (lambda (x) x)]`` can be type checked properly.
Otherwise, above list would have types ``[t0 -> t0, t1 -> t1]``
and not be considered homogeneous.

.. sc:: lhs

> tInfer (Pair a b) = do
>     tA <- tInfer a
>     tB <- tInfer b
>     s <- getSubst
>     return $ pair (apply s tA) (apply s tB)

To infer type of a pair, types of the 2 elements of the pair are
inferred and returned.

.. sc:: lhs

> tInfer (Ident x) = do
>     s <- getSubst
>     case Map.lookup x s of
>         Just ts -> do
>             t <- toType ts
>             s <- getSubst
>             return (apply s t)
>         otherwise -> throwErr ("not found in assumptions: " ++ x)

To type check a variable, type of the variable is looked up in
current Subst. If the variable has no bound type, error is thrown
with current SourcePos.
It is assumed that variables are only introduced from
lambda abstraction, let expression, and function definition.

.. sc:: lhs

> tInfer (Expr []) = return tUnit
> tInfer (Expr l) = tInfer (foldl1 App l)

Type of an empty expression is unit ``()``.
The expression is converted to App version before it is type inferred::

    (f a b c d e)
    ==> (App (App (App (App (App f a) b) c) d) e)

.. sc:: lhs

> tInfer (App f x) = do
>     tF <- tInfer f
>     tX <- tInfer x
>     v <- newTVar
>     let tF' = tX `fn` v
>     unify tF tF'
>     s <- getSubst
>     return (apply s v)

To infer type of App value, type of the function is inferred first (tF).
Then, type of the argument is inferred (tX).
Then type of the function is unified with ``tX -> a``
where a is a fresh type variable::

    App (a -> b -> c) (d -> e)
    ==> unify (a -> b -> c) ((d -> e) -> f) where f is fresh
        ==> [("a", d -> e), ("f", b -> c)]
    ==> b -> c

App value is never generated from the parser. Only Expr value
is converted to App value during type inference.

.. sc:: lhs

> tInfer (If pred true false) = do
>     tPred <- tInfer pred
>     unify tPred tBool
>     tTrue <- tInfer true
>     tFalse <- tInfer false
>     s <- getSubst
>     unify (apply s tTrue) (apply s tFalse)
>     s <- getSubst
>     return (apply s tFalse)

Type of if expression is inferred so that pred has type Bool
and types of true case (tTrue) and false case (tFalse) are unified.
The unified type of true case and false case is the type of the if
expression.

.. sc:: lhs

> tInfer (Lambda [] e) = tInfer e

Type of a lambda expression that doesn't abstract a variable
is just type of body expression.

.. sc:: lhs

> tInfer lam@(Lambda [x] e) = do
>     v <- newTVar
>     let sF = (x +-> TScheme [] v)
>     tE <- localSubst sF (tInfer e)
>     s <- getSubst
>     let domain = apply s v
>     let result = domain `fn` tE
>     return (apply s result)

Type of a lambda expression λx.e is inferred in an environment (Subst)
where x is bound to a fresh type variable.
After inferring type of e, the environment is updated to have
proper type of x (since x can appear in e).
Actual type of the lambda is ``type of x -> type of e``.

When inferring type of e, localSubst is used that runs type inference
on the given Subst, sF, instead of current global Subst.
localSubst stores types of variables that conflict with sF. Then it restores
them upon exit::

    localSubst [("x", Int)]
    ==> sF = [("x", Int)]
    ==> current subst  = [("x", Float), ...]
    ==> ("x", Float) is cached.
    ==> type inference is performed in the environment where
        x is mapped to Int
    ==> ("x", Float) is restored to current subst.

.. sc:: lhs

> tInfer lam@(Lambda params _) = if noDup params
>     then
>         tInfer (simplifyLambda lam)
>     else
>         throwErr ("duplicate argument: " ++ show lam)

When lambda has more than 1 parameter, it is turned to
a lambda with 1 parameter and type inferred::

    (lambda (x y z) e)
    ==> (lambda (x) (lambda (y) (lambda (z) e)))

noDup makes sure there's no duplicate parameter (such as ``(lambda (x x) e)``).
simplifyLambda turns multiple parameter lambda expression to
normal 1 parameter lambda expression.

.. sc:: lhs

> tInfer (Let [(k,v)] e) = do
>     tV <- tInfer v
>     s <- getSubst
>     let keys = Map.keys s
>     let vals = Map.elems s
>     let freeVs = (tv tV \\ keys) \\ tv vals
>     localSubst s (do
>         extendSubst (k +-> TScheme freeVs tV)
>         tE <- tInfer e
>         tK <- tInfer (Ident k)
>         unify tK tV
>         s' <- getSubst
>         return (apply s' tE))

For a let expression of the form ``let { k = v } e``,
type of v is inferred first (tV).
Since tV might contain type variables that are already in current Subst,
only those type variables that do not appear in the Subst are
stored in freeVs.
For example::

    (lambda (x) (let { x = x } x))
             |         |   |   +----- x of let      x4
             |         |   +--------- x of lambda   x3
             |         +------------- x of let      x2
             +----------------------- x of lambda   x1

While inferring type of x3 (x of lambda), it can get type variable t0
(during type inferencing the outer lambda,
x gets type variable t0, for example).
Then when extending Subst with ``x2 +-> t0``
(``k +-> TScheme freeVs tV`` in source code), x2 is bound to t0.
And, this will interfere type inference for x1.
So, freeVs only stores type variables that are not already found in
the current Subst.
After getting freeVs, type of e is inferred in local environment,
where k is bound to tV with freeVs universally quantified.
During inference of e, which can contain k, Subst is modified
to have enough information about k to infer type of k.
Type of k is inferred (tK) and is unified with tV.
During unification, Subst is modified to contain most general unifier
of tK and tV. Using the modified Subst, type of e (tE) is
finalized.

.. sc:: lhs

> tInfer (Let kvs e) = tInfer $ foldr (Let . (:[])) e kvs

When a let expression has more than 1 key value assignment
(such as ``let {x1 = v1, x2 = v2, ... } e``),
it is converted to a let expression with only 1 key value assignment.
And, type of the converted let expression is inferred::

    let {x1 = v1, x2 = v2, ..., xN = vN} e
    ==> let { x1 = v1 }
            (let {x2 = v2}
                ...
                (let {xN = vN} e))

This is possible because LIPL does not allow mutually recursive
let expressions, and key value assignments are evaluated in sequence.

.. sc:: lhs

> tInfer e@(FunDef name args body) = if noDup args
>     then
>         do
>             v <- newTVar
>             extendSubst (name +-> mkMonoType v)
>             tF <- tInfer (Lambda args body)
>             unify v tF
>             s <- getSubst
>             let result = apply s v
>             extendSubst (name +-> mkPolyType result)
>             return result
>     else
>         throwErr ("duplicate argument: " ++ show e)

For a function definition, parameters are checked to make sure
they don't contain duplicates.
Then, Subst is extended with function name bound to a new type variable.
mkMonoType is used because the function name should be instantiated
with one type only in the function body (for recursive functions).
Then, using parameters and function body, a lambda expression is formed
and type of the lambda is inferred (tF).
After unifying tF with the new type variable bound to function name (v),
type of the function is computed by applying current Subst to
the new type variable.
The current Subst is extended with the function name bound to the function
type.

.. sc:: lhs

> typeInfer v = typeInfer' v `E.catchError`
>     (\e -> case e of
>         Err _ _ -> E.throwError e
>         otherwise -> throwErr (show e))
>     where
>         typeInfer' (At _ e@(FunDef _ _ _)) = typeInfer' e
>         typeInfer' e@(FunDef name args body) = do
>             t <- M.liftM tSanitize (locally (tInfer e))
>             extendSubst (name +-> mkPolyType t)
>             return t
>
>         typeInfer' (At _ (Expr [e])) = typeInfer' e
>         typeInfer' (Expr [e]) = typeInfer' e
>         typeInfer' e = do
>             t <- locally (tInfer e)
>             return (tSanitize t)

typeInfer uses tInfer but after inferring type of given LIPL value,
it brings Subst to previous state (using locally function).
Only when the Val is function definition, Subst is extended
to register type of the function.
When an error is thrown during type inference, it is caught
and re-thrown. When the error caught does not already have
SourcePos, current SourcePos is retrieved and the error
re-thrown (using throwErr).

::

    ghci> :m + Control.Monad
    ghci> :t liftM
    liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r

liftM takes a transformation function (a function of type ``a -> b``)
and transforms the value inside a monad.



.. sc:: lhs

> toType (TScheme l t) = do
>     l' <- mapM (const newId) l
>     return $ subst (zip l l') t

toType instantiates a TScheme to a Type
by replacing universally quantified type variables with
new type variables.

::

    ghci> :t mapM
    mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]

mapM takes a function that returns a monad
and applies it to each element in a list.
The list now has monads.
Then it executes each monad in the list collecting
values of each monad in a list.

.. sc:: lhs

> locally action = do
>     s <- getSubst
>     n <- getN
>     result <- action
>     putSubst s
>     putN n
>     return result

locally caches current state of TI monad.
Then it executes the given action.
And it restores the cached state before returning.

.. sc:: lhs

> localSubst s action = do
>     sOrig <- getSubst
>     let cache = sOrig `Map.intersection` s -- store sOrig's types
>     extendSubst s
>     result <- action
>     s' <- getSubst
>     let ks = Map.keys s
>     let s'' = subtractMap s' ks @@ cache -- restore sOrig's types
>     putSubst s''
>     return result

localSubst extends current Subst with the given Subst, s.
When s contains conflicting entries, original types bound to
the conflicting entries are cached.
After running the given action, cached types are restored.

.. sc:: lhs

> defaultSubst :: Subst
> defaultSubst = toSubst [
>     ("Int", tInt)
>     , ("Float", tFloat)
>     , ("Bool", tBool)
>     , ("Char", tChar)
>     , ("Str", list tChar)
>     ]
> initialSubst = defaultSubst `Map.union` toSubst builtinSubst

initialSubst has types of built-in functions (``+``, ``-``, head, ...)
and base type constants (Int, Float, ...).

.. sc:: lhs

> clearSubst :: (MonadTI m) => m ()
> clearSubst = putSubst initialSubst

clearSubst clears current type environment by replacing it with
initialSubst.

.. sc:: lhs

> showS s = showSubst $ onlyNew s
> onlyNew s = s `Map.difference` initialSubst

showS is a convenience action that converts key-type mappings
that are not part of initialSubst to String.
