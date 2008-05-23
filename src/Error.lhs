=========
Error.lhs
=========

Module Error defines Err type that is used in the interpreter.

.. sc:: lhs

> module Error where
>
> import qualified Text.ParserCombinators.Parsec as P
> import qualified Text.PrettyPrint.HughesPJ as PP
> import Text.PrettyPrint.HughesPJ ( (<>), (<+>), ($$), ($+$) )
> import qualified Control.Monad.Error as E
>
> import LangData
>
> type ErrMsg = String
> data Err = Err P.SourcePos ErrMsg

Err in ``data Err`` is a type constructor. Err after ``=`` is
data constructor.
Type constructor is used to construct a type (in this case type Err).
And, data constructor is used to construct a value or data of some type.
So, a value of type Err can be constructed using Err data constructor.

.. sc:: lhs

> ppErr (Err pos msg) = PP.fsep [PP.text (show pos), PP.text msg]

ppErr turns a value of type Err into a value that can be pretty-printed
(looks nice when printed).

.. sc:: lhs

> instance Show Err where
>     show e = PP.render $ ppErr e

By making Err an instance of Show class, a value of type Err can be
converted to String and printed.
When a value of type Err is converted to String, ppErr is used
to make the string representation look nice.

.. sc:: lhs

> instance E.Error Err where
>     noMsg = Err initialPos "Error"
>     strMsg = Err initialPos

By making Err an instance of Error class, Err can be used
wherever an instance of Error class can be used.

Typeclass
=========

Haskell has notion of typeclasses.

A typeclass can be declared::

    class Foo a where
        bar :: a -> Int
        foo :: a -> String

The above declares a typeclass Foo that provides 2 functions: bar and foo.
All instances of Foo should implement those 2 functions::

    data Bar = Bar Int String

    instance Foo Bar where
        bar (Bar i _) = i
        foo (Bar _ s) = s

Bar is an instance of Foo and it implements bar and foo.
A value of type Bar can be used anywhere where class Foo is expected::

    func :: (Foo a) => a -> (String, Int)
    func a = (foo a, bar a)

    func (Bar 1 "1")
    ==> ("1", 1)

If there were another instance of Foo, it could be passed to func::

    data A = A
    instance Foo A where
        bar _ = 1
        foo _ = "1"

    func A
    ==> ("1", 1)

So, with typeclass, one can overload a function.
``func`` is overload to take a value of type A or Bar
(both are instances of class Foo).
