=========
Stack.lhs
=========

.. sectnum::
.. contents::

This file defines Stack module

.. sc:: lhs

> module Stack where


``type`` keyword can be used to create type synonyms::

    type MyInt = Int

makes MyInt an alias of built-in type Int.
MyInt can be used in place of Int.
MyInt is called a type constructor, just like Int.

.. sc:: lhs

> type Stack a = [a]

defines a type synonym, Stack.
Stack is synonym of ``[a]``, which is a list of a's.
a is a type variable. It can be instantiated with different types:

- ``[Int]`` or ``Stack Int`` is a list of Ints.
  The type variable a is instantiated with Int.
- ``[Float]`` or ``Stack Float`` is a list of Floats.
  The type variable a is instantiated with Float.
- ``[Bool]`` or ``Stack Bool`` is a list of Bools
  The type variable a is instantiated with Bool.

So, Stack is a type constructor that takes 1 parameter, a.
Once it has taken the parameter, it can construct a concrete type,
e.g. a list of Int.

Types and type constructors must start with an uppercase letter,
unlike normal functions.

One can annotate type of a function::

    functionName :: Type

tells the compiler that the function functionName has type Type.
``::`` is part of type annotation syntax.
``a :: B`` says, "a has type B".

.. sc:: lhs

> isEmpty :: Stack a -> Bool

So, isEmpty has type ``Stack a -> Bool``.
``a -> b`` means it is a function whose domain is type a and range is
type b (a function that takes a value of type a and returns a value of
type b).
Hence, isEmpty is a function that takes ``Stack a`` (a list of a's)
and returns Bool (boolean).

.. sc:: lhs

> isEmpty = null

After type annotation of a function, definition of the function must follow.
So, isEmpty is same as null, which is a Haskell function that returns
True if the list is empty, False otherwise::

    ghci> :t null
    null :: [a] -> Bool
    ghci> null ""
    True
    ghci> null []
    True
    ghci> null [1]
    False

``:t expr`` is GHCi command that prints type of expr.

.. sc:: lhs

> pop :: Stack a -> (a, Stack a)
> pop (x:xs) = (x, xs)

defines function pop that takes a list of a's and returns
a pair of a and a list of a's.

``(a, b)`` means a pair of a value of type a and a value of type b::

    ghci> (1, 1) :: (Int, Float)
    (1, 1.0)

You can also annotate type of an expression with ``::`` .

Definition of pop uses pattern.
A pattern can be a literal or
something that starts with (or includes) a function
(normal function, type constructor, data constructor, operator).
``:`` in ``(x:xs)`` is a data constructor for list types::

    ghci> :t (:)
    (:) :: a -> [a] -> [a]

A data constructor is a function that starts with
``:`` and continues with special characters, or a function
that starts with an uppercase letter and continues with normal function name.
For example, ``:->``, ``:===-?``, Hello, He''ll__o are data constructors.

``(x:xs)`` is same as ``((:) x xs)``.
``pop (x:xs)`` matches function calls like::

    pop [1,2,3]
    pop "abc"

because::

    pop [1,2,3] ==> 1 : [2,3]        ==> (1, [2,3])
                    |     +----- xs ------|----+
                    +----------- x  ------+

    pop "abc" ==> 'a' : "bc"         ==> ('a', "bc")
                   |     +------ xs -------|----+
                   +------------ x  -------+


.. sc:: lhs

> push :: a -> Stack a -> Stack a
> push v s = v : s

push matches calls like::

    push 1 [2,3]         ==> [1,2,3]
         |   +------- s ------|--+
         +----------- v ------+

    push 'a' "bc"        ==> "abc"
          |   +------ s ------|+
          +---------- v ------+

::

    ghci> :l Stack
    ghci> pop "abc"
    ('a',"bc")
    ghci> push 'a' "bc"
    "abc"
    ghci> pop []
    *** Exception: Stack.lhs:87:2-21: Non-exhaustive patterns in function pop

``pop []`` throws an exception because pop is defined only on
``(x:xs)``, a list of at least one element.
So, ``[]`` won't match ``x:xs`` and causes an exception.
