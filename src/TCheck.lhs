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
When something fails, saved SourcePos can be retrieved and reported.

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
>         otherwise -> do
>             pos <- getSourcePos
>             E.throwError
>                 $ Err pos ("primitive function not found: " ++ x)

For built-in functions (PrimFun),
find the type of it in the current substitution (type variable
and type mapping).
When it's not found, throwError.
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
>             do
>                 pos <- getSourcePos
>                 E.throwError
>                     $ Err pos ("not homogeneous list: " ++ show (List l))

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
>         otherwise -> do
>             pos <- getSourcePos
>             E.throwError $ Err pos ("not found in assumptions: " ++ x)

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

To infer type of App value, type of the function is inferred first.
Then, type of the argument is inferred.
Then type of the function is unified with ``type of argument -> a``
where a is a fresh type variable::

    App (function of type a -> b -> c) (function of type d -> e)
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
and types of true case and false case are unified.
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
proper type of x.
Actual type of the lambda is ``type of x -> type of e``.

When inferring type of e, localSubst is used that runs type inference
on the given Subst, sF, instead of current global Subst.
localSubst stores types of variables that conflict with sF then restores
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
>         do
>             pos <- getSourcePos
>             E.throwError $ Err pos ("duplicate argument: " ++ show lam)

> tInfer (Let [(k,v)] e) = do
>     tV <- tInfer v
>     s <- getSubst
>     let keys = Map.keys s
>     let vals = Map.elems s
>     let freeVs = (tv tV \\ keys) \\ tv vals
>
>     s <- getSubst
>     localSubst s (do
>         extendSubst (k +-> TScheme freeVs tV)
>         tE <- tInfer e
>         sE <- getSubst
>         tK <- tInfer (Ident k)
>         sK <- getSubst
>         unify tK tV
>
>         s <- getSubst
>         return (apply s tE))
>
> tInfer (Let kvs e) = tInfer $ foldr (Let . (:[])) e kvs
>
> {-
> tInfer (Seq _ e2) = do
>     tE2 <- tInfer e2
>     return tE2
> -}
>
> tInfer e@(FunDef name args body) = if noDup args
>     then
>         do
>             v <- newTVar
>             extendSubst (name +-> TScheme [] v)
>             tF <- tInfer (Lambda args body)
>             unify v tF
>             s <- getSubst
>             let result = apply s v
>             extendSubst (name +-> mkPolyType result)
>             return result
>     else
>         do
>             pos <- getSourcePos
>             E.throwError $ Err pos ("duplicate argument: " ++ show e)
>
> typeInfer (At _ e@(FunDef _ _ _)) = typeInfer e
> typeInfer e@(FunDef name args body) = do
>     t <- M.liftM tSanitize (locally (tInfer e))
>     extendSubst (name +-> mkPolyType t)
>     --s <- getSubst
>     return t
>
> typeInfer (At _ (Expr [e])) = typeInfer e
> typeInfer (Expr [e]) = typeInfer e
> typeInfer e = do
>     t <- locally (tInfer e)
>     return (tSanitize t)
>
>
> {-
> tInfer (Let kvs e) = tInfer $ Expr (Lambda (keys kvs) e : vals kvs)
>     where
>         keys = map fst
>         vals = map snd
> -- Let [(x1, e1), (x2,e2), ..., (xN,eN)] expr
> -- ==> Expr [Lambda [x1, x2, ..., xN] expr, e1, e2, ...]
> -}
>
>
>
>
> mkFunType [] = do
>     v <- newTVar
>     return v
>
> mkFunType (_:xs) = do
>     v <- newTVar
>     rest <- mkFunType xs
>     return (v `fn` rest)
>
>
> tInferList :: (MonadTI m, MonadPos m, E.MonadError Err m) => [Val] -> m [Type]
> tInferList = mapM tInfer
>
>
> toType (TScheme l t) = do
>     l' <- mapM (const newId) l
>     return $ subst (zip l l') t
>
> showS s = showSubst $ onlyNew s
> onlyNew s = s `Map.difference` initialSubst
>
> locally action = do
>     s <- getSubst
>     n <- getN
>     result <- action
>     putSubst s
>     putN n
>     return result
>
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
>
> tUpdateTVars e = do
>     let vs = getTVars e
>     vs' <- mapM (const newId) vs
>     return $ subst (zip vs vs') e
>
>
>
> simplifyLambda lam@(Lambda [] e) = lam
> simplifyLambda lam@(Lambda [x] e) = lam
> simplifyLambda lam@(Lambda (x:xs) e) =
>     Lambda [x] (simplifyLambda (Lambda xs e))
> simplifyLambda (Expr [x]) = simplifyLambda x
>
>
> {-
> ty input = case parseSingle input of
>     Right v -> case runTI (tInfer v) initialSubst 0 of
>         (s,i,t) -> t --tSanitize t
>     Left err -> error (show err)
>     where
>         prettySubst s = s `Map.difference` initialSubst
>
> tyi input = case parseSingle input of
>     Right v -> case runTI (tInfer v) initialSubst 0 of
>         (s,i,t) -> putStrLn
>             $ showSubstType (prettySubst s) t -- (tSanitize t)
>     Left err -> putStrLn (show err)
>     where
>         prettySubst s = s `Map.difference` initialSubst
>         -- (s,i,t) -> putStrLn $ showSubstType (s \\ initialSubst) t
>
> tyim input = case parseMultiple "" input of
>     Right l -> case runTI (tInferList l) initialSubst 0 of
>         (s, i, t) -> mapM_ (putStrLn . show) (map tSanitize t)
>     Left err -> putStrLn (show err)
>
> -}
>
> -- ti (Lambda ["x"] (Lambda ["x"] (App (App (PrimFun "+") (Ident "x")) (Ident "x"))))
>
>
> printRunTIResult (s,i,t) = putStrLn $ show t
>
> theta = [("X", TVar "a"), ("Y", TVar "b"), ("Z", TVar "Y")]
> eta = [("X", TApp (TVar "f") (TVar "Y")), ("Y", TVar "Z")]
> -- theta @@ eta = [("X",f b),("Z",Y)]
>
> {-
> tCheck :: Val -> Either String Type
> tCheck v = case runTI (tInfer v) initialSubst 0 of
>     (s, i, t) -> Right t
> -}
>
> defaultSubst :: Subst
> defaultSubst = toSubst [
>     ("Int", tInt)
>     , ("Float", tFloat)
>     , ("Bool", tBool)
>     , ("Char", tChar)
>     , ("Str", list tChar)
>     ]
> initialSubst = defaultSubst `Map.union` toSubst builtinSubst
>
> clearSubst :: (MonadTI m) => m ()
> clearSubst = putSubst initialSubst
>
>
>
