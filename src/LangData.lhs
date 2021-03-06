============
LangData.lhs
============

.. sectnum::
.. contents::

LangData module defines Val type, which represent
values used inside LIPL interpreter.

``module ModuleName (func1, func2) where``
creates a module ModuleName where func1, func2 are exported
(``(fun1, func2)`` is called an export list).
So, in another module::

    import ModuleName

    f = func1 -- this is fine
    g = func2 -- also fine because ModuleName exports these.
    h = func3 -- is not fine because func3 is not exported by
                 the module, ModuleName.

.. sc:: lhs

> module LangData ( Val (..)
>     , Env, emptyEnvStack, emptyEnv, EnvStack, showEnv, showEnvs
>     , KeyValList, Key, initialPos
>     , ppValList, ppVal, toStr
>     ) where

So, LangData module exports ``Val (..)``, Env, emptyEnvStack, ...etc.
``Val (..)`` means all data constructors of type Val are exported too.

.. sc:: lhs

> import qualified Text.PrettyPrint.HughesPJ as PP
> import Text.PrettyPrint.HughesPJ ( (<>), (<+>), ($$), ($+$) )
> import qualified Text.ParserCombinators.Parsec as P
> import qualified Text.ParserCombinators.Parsec.Pos as P
> import qualified Data.Map as Map
> import qualified Data.List as List
>
> import Stack
>
> type Key = String
> type KeyVal = (Key, Val)
> type KeyValList = [KeyVal]
> initialPos = P.initialPos ""

Key, KeyVal, KeyValList are type synonyms for convenience.
initialPos is default source position used in the interpreter
``(line 1, column 1)``.

.. sc:: lhs

> ppKeyVal (k, v) = ppVal (Ident k)
>     <+> PP.text "="
>     <+> ppVal v

ppKeyVal takes a pair (Key, Val) and converts them to Doc
(PrettyPrint data type that is used to pretty print values)::

    ppKeyVal ("a", Int 1)
    ==> a = 1

``ppKeyVal ("a", Int1)`` will eventually be printed as: ``a = 1``.

.. sc::haskell

> ppKeyValList :: KeyValList -> PP.Doc
> ppKeyValList l = ppDict $ map ((PP.empty $$) . ppKeyVal) l
> ppDict l = PP.braces $ PP.fsep $ PP.punctuate PP.comma l

ppKeyValList prints::

    [("a", Int 1), ("b", Int 2), ("c", Str "hello")]
    as
    {a = 1, b = 2, c = "hello"}

.. sc:: lhs

> type Env = Map.Map Key Val
> type EnvStack = Stack Env
> emptyEnv = Map.empty
> emptyEnvStack = [emptyEnv]

EnvStack is a Stack of (hence a list of) Env (or Map.Map Key Val).

.. sc:: lhs

> showEnv env = show $ ppEnv env
> showEnvs envs = PP.render
>     ((PP.fsep $ map ppEnv envs) $+$ PP.int (length envs))
> ppEnv env = ppKeyValList  $ Map.toList env
> ppValList = map ppVal
> ppStrList = map PP.text
> ppArgs args = PP.parens $ PP.fsep $ ppStrList args

Above funcitons all facilitate pretty printing of Env, EnvStack, and Val.

Many functions from PrettyPrint library (PP.render, PP.text, ...etc)
are documented at:
http://haskell.org/ghc/docs/latest/html/libraries/pretty/Text-PrettyPrint.html

Hoogle_ is a great resource to look up a function and find
documentation about it.

.. _Hoogle: http://www.haskell.org/hoogle/

.. sc:: lhs

> type Name = String
> type Params = [Name]
> type RemainingArgs = Int

One can create ADT (abstract data type) with::

    data TypeCon = DataCon1 Int
        | DataCon2 String Int
        | ...

Above creates an ADT of type TypeCon that can be
constructed using DataCon1, DataCon2, ...etc.
Haskell ADT's are sum of product types.

.. sc:: lhs

> data Val =
>     Null
>     | At P.SourcePos Val
>     | Ident { unpackIdent :: Name }
>     | Int { unpackInt :: Integer }
>     | Float { unpackFloat :: Double }
>     | Bool { unpackBool :: Bool }
>     | Char { unpackChar :: Char }
>     | Str { unpackStr :: String }
>     | FunDef Name Params Val
>     | PrimFun Name
>     | Lambda Params Val
>     | Fun Env Params Val -- parser don't generate this
>     | Prim Name RemainingArgs [Val] -- parser don't generate this
>     | List [Val]
>     | Dict KeyValList
>     | Pair Val Val
>     | Expr [Val]
>     | App Val Val
>     | Let KeyValList Val
>     | If Val Val Val
>     deriving (Ord, Eq)

Type Val is defined above. Val represents values used inside the interpreter.
Parser will create values of type Val from String using
At data constructor, which stores source code position along with Val.
So, upon error, the interpreter can print position of error.

To construct a value of type Val, one can use
any of the given data constructors above::

    Null
    Ident "foo"
    List [Null, Int 1, Float 2.35, Ident { unpackIdent = "bar"}]
    ...

These all construct a value of type Val.
Note that Val is a type constructor and can't be used to construct
a value (data) of type Val::

    ghci> :l LangData
    ghci> Val
    <interactive>:1:0: Not in scope: data constructor `Val'

A data constructor ``Bool Bool`` is same as ``Bool { unpackBool :: Bool }``.
The latter is using record syntax.
If a data constructor is defined using record syntax,
each record accessor becomes a top level function::

    data Foo = Foo { a :: Int, b :: String }

    a (Foo 1 "hello")
    ==> 1

    b (Foo { a = 2, b = "bye" })
    ==> "bye"

One benefit of using record syntax is that modification
of one field is easy::

    (Foo 1 "hello") { b = "bye" }
    ==> Foo 1 "bye"

    instead of

    case (Foo 1 "hello") of
        Foo a' _ -> Foo a' "bye"
    ==> Foo 1 "bye"

Deriving clause (``deriving (Ord, Eq)``) makes values of type Val
to be comparable (can use ``==``, ``>``, ... operators on Val).

Ord and Eq are typeclasses in Haskell.
A typeclass declares functions.
One can make a type (such as Val) an instance of a typeclass
by implementing those declared functions (or derive it using
``derive`` kewword).
To instantiate a typeclass, one can use ``instance`` keyword::

    instance Class1 Type1 where
        funcClass1DeclaresA = ...
        funcClass1DeclaresB = ...

Type1 is now an instance of Class1.

.. sc:: lhs

> instance Show Val where show = PP.render . ppVal

By making Val an instance of Show class, values of type Val
can be converted to String and printed.

.. sc:: lhs

> ppVal Null = PP.text "Null"
> ppVal (At _ e) = ppVal e
> ppVal (Ident s) = PP.text s
> ppVal (Int i) = PP.integer i
> ppVal (Float f) = PP.double f
> ppVal (Bool b) = PP.text $ show b
> ppVal (Char c) = PP.text $ show c
> ppVal (Str s) = PP.text $ show s
> ppVal (Fun env args body) = PP.parens
>     $ PP.fsep [PP.text "function"
>         , ppArgs args, ppVal body]
> ppVal (Lambda args body) = PP.parens
>     $ PP.fsep [PP.text "lambda", ppArgs args, ppVal body]
> ppVal (FunDef name args body) = PP.parens
>     $ PP.fsep [PP.text "def", PP.text name
>         , ppArgs args
>         , ppVal body]
> ppVal (PrimFun name) = PP.text name
> ppVal (Prim name _ _) = PP.text "<builtin:"
>     <+> PP.text name <> PP.text ">"
> ppVal (List l@(Char c:xs)) = ppVal (toStr l)
> ppVal (List xs) = PP.brackets
>     (PP.fsep $ PP.punctuate PP.comma (ppValList xs))
> ppVal (Dict xs) = ppKeyValList xs
> ppVal (Pair a b) = PP.parens
>     (PP.fsep $ PP.punctuate PP.comma [ppVal a, ppVal b])
> ppVal (Expr xs) = PP.parens (PP.fsep $ ppValList xs)
> ppVal (If pred ifCase elseCase) = PP.parens
>     $ PP.fsep [PP.text "if", ppVal pred, ppVal ifCase, ppVal elseCase]
> ppVal (Let env val) = PP.parens
>     $ PP.fsep [PP.text "let", ppKeyValList env, ppVal val]
> ppVal (App f x) = PP.parens (PP.fsep [ppVal f, ppVal x])

ppVal is used to pretty print values of type Val.
It is defined case by case for different patterns.
From the top to bottom, given a Val, the Val is matched
against Null, ``At _ e``,  ``Ident s``, ..., ``App f x``.
When a match is found, the body (the expression after ``=``) is evaluated
in an environment where variables in the pattern are bound to actual values
matched.
For example::

    ppVal (Prim "+" 3 [Int 1, Str "hello"])

will fail to match all patterns until it matches the pattern
``ppVal (Prim name _ _)``  (``_`` matches anything).
So, the body expression of the pattern will be evaluated with "+"
bound to the variable, name::


    function call: ppVal (Prim "+" 3 [Int 1, Str "hello"])
                     ^     ^    ^  ^   ^
                     |     |    |   \ /
                     |     |    |   | |
    pattern:       ppVal (Prim name _ _)
                                |   | +--> _ is not a variable
                                |   +----> _ is not a variable
                                +--------> name gets "+"

ppVal could have written using ``case`` expression::

    ppVal v = case v of
        Null    -> ...
        At _ e  -> ...
        Ident s -> ...
        ...
        App f x -> ...



.. sc:: lhs

> toStr [] = Str ""
> toStr (Char c:xs) = let Str xs' = toStr xs
>     in
>         Str (c:xs')

toStr converts a list of Char's to Str::

    toStr [Char 'a', Char 'b', Char 'c']
    ==> Str "abc"

