========
Type.lhs
========

.. sectnum::
.. contents::

Type module defines useful functions (such as unification)
for type inference and a way to represent types.

.. sc:: lhs

> module Type where
>
> import qualified Data.List as List
> import qualified Data.Map as Map
> import qualified Text.PrettyPrint.HughesPJ as PP
> import Text.PrettyPrint.HughesPJ (
>     (<>), (<+>), ($$), ($+$) )
>
> import Utils
>
> type Id = String
>
> data Type = TVar { getId :: Id }
>     | TConst { getId :: Id }
>     | TApp Type Type
>     deriving (Eq, Ord)

Type is represented by values of type Type.
TVar constructs values that represent type variables.
TConst represents type constants.
TApp represents application of a type to another.
For example, ``Int -> [Char]`` can be represented as::

    TApp (TApp (TConst "->") (TConst "Int"))
         (TApp (TConst "[]") (TConst "Char"))

Type derives Eq and Ord class so that values of type Type can
be compared.

.. sc:: lhs

> instance Show Type where
>     show = PP.render . ppType
>
> ppType (TVar v) = PP.text v
> ppType (TConst c) = PP.text c
> ppType (TApp (TApp (TConst "(,)") a) b) =
>     PP.parens (PP.fsep [ppType a, PP.text ",", ppType b])
> ppType (TApp (TConst "[]") a) = PP.brackets (ppType a)
> ppType (TApp (TConst "{}") a) = PP.braces (ppType a)
> ppType (TApp (TApp (TConst "->") a) b) =
>     PP.fsep [if isFun a then PP.parens (ppType a) else ppType a
>         , PP.text "->", ppType b]
> ppType (TApp a b) = ppType a <+> ppType b

Type is an instance of Show class. And, ppType is defined so that
a value of type Type can be shown in pretty way.
For example, when a Type value has pattern::

    TApp (TApp (TConst "(,)") a) b

this represents a pair ``(a,b)``. So, it is converted to
Doc (pretty printer type) accordingly.

For function types::

    TApp (TApp (TConst "->") a) b

parenthesis used when a is a function type too.
So, it can insert parenthesis like ``(a) -> b`` when a is a function.
Of course, a would get expended to something like: ``(x -> y) -> b``
when a were of type ``x -> y``.

.. sc:: lhs

> isFun (TApp (TApp (TConst "->") _) _) = True
> isFun _ = False

> getTVars v = List.nub (getTVars' v)
>     where
>         getTVars' (TVar v) = [v]
>         getTVars' (TApp t1 t2) = getTVars' t1 ++ getTVars' t2
>         getTVars' _ = []
>
> tSanitize t =
>     subst [(v, "t" ++ show i) | (i,v) <- zip [0..] (getTVars t)] t

getTVars returns type variables in a type expression::

    a -> Int -> b -> Char
    ==> [a,b]

tSanitize normalizes type variables by replacing them in a uniform way.
All type variables in a type expression become t0, t1, ...etc.
To do so, tt first creates ``[(0, var0), (1, var1), ..., (N, varN)]``,
where var0, var1, ..., varN are type variables in the type expression
(they can be irregular like [a,x,t0]).
Then it creates ``[(var0, t0), (var1, t1), ..., (varN, tN)]``,
where t0, t1, ..., tN are regular (they are always [t0, t1, ...]).
And, it actually makes substitution for each varI with tI::

    a -> Int -> b -> Char
    ==> t0 -> Int -> t1 -> Char

.. sc:: lhs

> tEq t1 t2 = tSanitize t1 == tSanitize t2

Using tSanitize, it is possible to compare equality of 2 type expressions::

    a -> b == x -> y
    ==> False

    a -> b `tEq` x -> y
    ==> tSanitize (a -> b) == tSanitize (x -> y)
    ==> t0 -> t1 == t0 -> t1
    ==> True

.. sc:: lhs

> subst dict t@(TVar v) = case lookup v dict of
>     Just v' -> TVar v'
>     Nothing -> t
> subst dict (TApp t1 t2) = TApp (subst dict t1) (subst dict t2)
> subst dict t = t

subst actually replaces all type variables in the given type expression
according to the association list.

.. sc:: lhs

> tUnit = TConst "()"
> tChar = TConst "Char"
> tInt = TConst "Int"
> tFloat = TConst "Float"
> tBool = TConst "Bool"
> tList = TConst "[]"
> tArrow = TConst "->"
> tDict = TConst "{}"
> tTuple2 = TConst "(,)"

Above are short hands for built-in type constants: Char, Int, ...

.. sc:: lhs

> a `fn` b = TApp (TApp tArrow a) b

fn function makes a function type, given 2 types::

    tInt `fn` tChar
    ==> Int -> Char

Backtics (``` ```) can be used to turn a function to an operator, just like
parenthesis can turn an operator to a function::

    (+) 1 2
    ==> 1 + 2
    ==> 3

    add 1 2
    ==> 1 `add` 2
    ==> 3

.. sc:: lhs

> list = TApp tList
>
> pair a b = TApp (TApp tTuple2 a) b

list and pair are helper functions (like fn function) to create
list-of types and pair types.

.. sc:: lhs

> -- type abstraction
> -- ((TScheme [x,y] ([y] -> x)) Int Char) ==> [Char] -> Int
> data TScheme = TScheme [Id] Type
>     deriving (Eq, Ord)
>
> instance Show TScheme where
>     show = PP.render . ppTScheme
>
> ppTScheme (TScheme l t) = PP.fsep [ppQuantification l, ppType t]
>
> ppQuantification l =  if null l
>     then
>         PP.empty
>     else
>         PP.fsep [PP.text "forall", PP.fsep (map PP.text l), PP.text "."]

TScheme holds a list of free type variables and a type.
Free type variables can be instantiated many times::

    id :: a -> a
    (id 1, id "1")

The type variable a will be instantiated to Int for ``id 1``.
Then, type if id would be ``Int -> Int``.
So, ``id "1"`` becomes illegal because id now expects an Int, not a String.
So, the type variable a in the type of id should be instantiated multiple
times (once for Int and then for String).

Using TScheme, type of id can be represented as::

    id :: TScheme ["a"] (TVar "a" `fn` TVar "a")

    instead of

    id :: TVar "a" `fn` TVar "a"

During type instantiation, TScheme can tell the type variable "a"
can be instantiated again.

.. sc:: lhs

> type Subst = Map.Map Id TScheme
>
> ppIdTScheme (i, ts) = PP.fsep [PP.text i, PP.text "::", ppTScheme ts]
> ppIdTSchemeList = map ((PP.empty $$) . ppIdTScheme)
> ppSubst l = PP.braces $ PP.fsep
>     $ PP.punctuate PP.comma (ppIdTSchemeList $ Map.toList l)
> showSubst = PP.render . ppSubst

Subst is a Map of Id and TScheme.
It stores which type variable has which type.

.. sc:: lhs

> nullSubst = Map.empty
>
> mkMonoType t = TScheme [] t
> mkPolyType t = TScheme (tv t) t

nullSubst is empty environment where it has no information about which
type variable is bound to which type.

mkMonoType creates a TScheme whose type variables can be instantiated
only once.
mkPolyType creates a TScheme whose type variables can be instantiated
multiple times.
tv function used in mkPolyType returns a list of type variables in a type.

.. sc:: lhs

> (+->) :: Id -> TScheme -> Subst
> v +-> ts = Map.fromList [(v, ts)]

``a +-> b`` builds a singleton Subst where type variable a is
mapped into type (scheme) b.

.. sc:: lhs

> fromIdType = map (\(k,v) -> (k, mkPolyType v))
>
> toSubst :: [(Id, Type)] -> Subst
> toSubst = Map.fromList . fromIdType

fromIdType converts ``[(Id, Type)]`` to
``[(Id, TScheme)]`` in a way that each Type bound to Id
is polytype (type variables can be instantiated more than once).
And toSubst converts ``[(Id, Type)]`` to Subst
such that each Type becomes polytype.

.. sc:: lhs

> class Types t where
>     apply :: Subst -> t -> t
>     tv :: t -> [Id]

Class Types provide 2 functions:

apply
    takes a Subst and a type and applies the Subst to the type
    by replacing all type variables in the type with types mapped
    according to the Subst.

tv
    takes a type and returns all type variables in it.

.. sc:: lhs

> instance Types Type where
>     apply s v@(TVar u) = case Map.lookup u s of
>         Just (TScheme _ t) -> apply s t
>         Nothing -> v
>     apply s (TApp f a) = TApp (apply s f) (apply s a)
>     apply s t = t
>
>     tv (TVar u) = [u]
>     tv (TApp f a) = tv f `List.union` tv a
>     tv _ = []

Type is an instance of Types.
So, one can apply a Subst to a Type, and get all type variables
from a Type.

.. sc:: lhs

> instance (Types a) => Types [a] where
>     apply s = map (apply s)
>     tv = List.nub . concat . map tv

A list of Types is also Types.

.. sc:: lhs

> instance Types TScheme where
>     apply s (TScheme l e) = TScheme l $ apply (s `subtractMap` l) e
>     tv (TScheme l e) = tv e

TScheme is also Types.

.. sc:: lhs

> s1 @@ s2 = Map.union s2 s1

``@@`` operator is composition of 2 Subst's::

    apply (s1 @@ s2) t
    ==> apply s1 (apply s2 t)

Since Map.union prefers first argument in case of duplicate key,
s2 has precedence::

    ghci> let s1 = Map.fromList [("a",1), ("b",2)]
    ghci> let s2 = Map.fromList [("a",2)]
    ghci> s1 @@ s2
    fromList [("a",2), ("b",2)]

In case s2 maps type variable a to Int and s1 maps type variable a to Char,
``s1 @@ s2`` maps type variable a to Int.

.. sc:: lhs

> mgu (TApp f1 a1) (TApp f2 a2) = do
>     s1 <- mgu f1 f2
>     s2 <- mgu (apply s1 a1) (apply s1 a2)
>     return (s2 @@ s1)
> mgu (TVar v) t = varBind v t
> mgu t (TVar v) = varBind v t
> mgu (TConst c1) (TConst c2)
>     | c1 == c2 = return nullSubst
> mgu _ _ = fail "types do not unify"
>
> varBind u t
>     | t == TVar u = return nullSubst
>     | u `elem` tv t = fail "occurs check fails"
>     | otherwise = return (u +-> TScheme [] t)

mgu takes 2 types and finds the most general unifier::

    mgu (a -> b) (Int -> b)
    ==> [(a, Int)]

The most general unifer of 2 types
is a Subst that can be applied to the 2 types to result in the same type::

    ghci> :l Type
    ghci> let t1 = TVar "a" `fn` TVar "b"
    ghci> t1
    a -> b
    ghci> let t2 = tInt `fn` TVar "a"
    ghci> t2
    Int -> a
    ghci> mgu t1 t2
    fromList [("a", Int), ("b", Int)]
    ghci> it -- value of the last expression (GHCi variable)
    fromList [("a", Int), ("b", Int)]
    ghci> apply it t1 `tEq` apply it t2
    True

varBind function is helper of mgu.
When mgu has to find unifier of a type variable and a type,
it makes sure the type doesn't contain the type variable.
Otherwise, the type can't be bound to the type variable::

    ghci> let t1 = TVar "a"
    ghci> let t2 = TVar "b" `fn` TVar "a"
    ghci> t1
    a
    ghci> t2
    b -> a
    ghci> mgu t1 t2
    *** Exception: user error (occurs check fails)

For ``a`` and ``b -> a`` to be unified, there must be a Subst
that can be applied to both and make them the same type.
Since the type variable a appears on both types, there can't be such Subst.
So, mgu fails with an exception saying that occurs check fails.

Monad
=====

mgu is a monadic function.
Unlike normal function, monadic function returns a value using
``return`` function: ``return 1``, for example.
The return function puts the value inside the monad.

To retrieve the value inside a monad, one can use ``<-``::

    ghci> s <- mgu (TVar "a") tInt
    fromList [("a", Int)]
    ghci> s
    fromList [("a", Int)]

GHCi itself is running inside a monad called IO. That's why ``<-``
works::

    ghci> let f = s <- mgu tInt (TVar "a")
    <interactive>:1:10: parse error on input `<-'

One can know a function is monadic when the function uses return function,
``<-``, or do notation::

    mgu (TApp f1 a1) (TApp f2 a2) = do
        s1 <- mgu f1 f2
        s2 <- mgu (apply s1 a1) (apply s1 a2)
        return (s2 @@ s1)

do notation looks like a sequence of expressions while a normal function body
is just one expression::

    f x = do
        expr1
        expr2
        ...

    f x = expr

do notation converts those aligned expression into one expression::

    do
        expr1
        expr2
        ...
        exprN

    ==> expr1 >> expr2 >> ... >> exprN

where ``>>`` is a monadic bind operator.
Bind operation is similar to function composition operation (``.`` is
function composition operator in Haskell)::

    (f . g . h) x
    ==> f (g (h x))
    ==> calculate h, g, and f.

    h' >> g' >> f'
    ==> compute h', g', and f'.

Another monadic bind operator is ``>>=``::

    f >>= (\ resultOfF -> g) >>= (\ resultOfG -> h)
    ==> compute f, pass its result (inside monad) to
        (\ resultOfF -> g), which is a lambda that takes the result
        and computes g (that might use the result).
        Pass the resultant value of the lambda (also inside monad)
        to (\ resultOfG -> h) that takes the result and computes
        h (that might use the result).

If one does not want to align expressions in do block, one can use
explicit braces and semicolons::

    do {
        expr1; expr2
      ;
     expr3;
        expr4;      expr5;
        ...
        exprN; }


