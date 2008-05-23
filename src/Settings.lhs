============
Settings.lhs
============

.. sectnum::
.. contents::

Literate Haskell
================

A Haskell program is made of multiple modules.
A module can be defined in a file.
Name of the file can end with ``.hs`` or ``.lhs``.
When it ends with ``.lhs``, the file is a literate Haskell source code:
lines that start with ``>`` are considered to be Haskell source code.
Other lines are ignored.
In literate Haskell, one can enclose Haskell source code
in ``\begin{code}`` and ``\end{code}``, instead of prefixing
each line with ``>``. But one must choose either of the two ways
to flag source code portion of the file.

This is a literate Haskell file that prefixes each source code line
with ``>``.

Comments
========

Anything after ``--`` until new line is comment.
Anything inside ``{- -}`` is comment.
For example::

    -- this is a single line comment
    {- this is a multiple line comment
        {- it can be nested -} -}

Module
======

To define a module, one can use::

    module ModuleName where
    -- module body

that defines a module named ModuleName.
``module`` and ``where`` are keywords.

.. sc:: lhs

> module Settings where

A module called Settings is defined in this file.

Function Definition
===================

A module has many top-level functions.
One can define a top-level function with::

    functionName = functionBody

that defines a function functionName.
Anywhere where functionBody appears, one can replace it with functionName.
For example::

    f = 1000
    g = 1000

can be rewritten as::

    f = g -- replacing 1000 with g
    g = 1000

Note that one would not need to define g before it can be used
(g is used before it is defined).

.. sc:: lhs

> sBASEDIR = ".lipl"
> sPRELUDE = "core.lipl"
> sLANGNAME = "LIPL"
> sPATH = sLANGNAME ++ "PATH"

So, these define 4 functions: sBASEDIR, sPRELUDE, ...
A function name should start with a lowercase letter.
Then, uppercase letters and ``'`` or ``_`` can follow::

    aPple, aP_pLE, apple', a_ppL''_'_E

are all valid function names.

Operators
=========

A function can start with one of these::

    :!#$%&*+./<=>?@\\^|-~

and continue with one of those.

Then, the function is an operator.
Usually, a function application is written as ``f a b``.
But for operators, it is written as ``a + b``, where ``+`` is an operator.
A function can be turned into an operator by enclosing it in ``` ```::

    f a b
    ==> a `f` b

Similarly, an operator can be turned into a function by enclosing it in
``( )``::

    a + b
    ==> (+) a b

``++`` is concatenation operator that concatenates two lists::

    sLANGNAME ++ "PATH"
    ==> "LIPLPATH" where sLANGNAME is "LIPL"

So, strings in Haskell are list of characters.
Strings can be written as::

    ['a', 'p', 'p', 'l', 'e']
    or
    "apple"

They represent the same string.
A character is enclosed in ``' '``. A string is enclosed in ``" "``.

``a ==> b`` means that a can be transformed to b. Or, b follows
from a.
It is not Haskell. Just a notation used in this document.

Convention
==========

``shell>`` is a shell prompt (command line)::

    shell> cd ..

means to type ``cd ..`` at command line prompt.

``ghci>`` is a GHCi prompt::

    ghci> 1 + 2
    3

means to type ``1 + 2`` at GHCi prompt. And it also shows what GHCi
prints when enter key is pressed.
To start GHCi::

    shell> ghci
    ghci>

``lipl>`` is a LIPL prompt::

    lipl> (+ 1 2)
    type: Int
    3

means to type ``(+ 1 2)`` at LIPL propmpt. And it shows
what LIPL interpreter prints.

::

    shell> cd path/to/Settings.lhs
    shell> ghci
    ghci> :l Settings

The last command will load Settings module (the module this file defines).
After loading the module, the prompt will change to ``*Settings>``.
However, ``ghci>`` will still be used through out this document.

::

    ghci> sPATH -- Settings module is already loaded.
    "LIPLPATH"



