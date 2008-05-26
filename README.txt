===========================================
LIPL: a Little Idiotic Programming Language
===========================================

Documentation
=============

Documentation is at doc/index.html

If doc/index.html is not there::

    shell> python doc/tools/gen.py

To run gen.py, you need Python, Docutils, and Pygments.

Install
=======

::

    shell> runhaskell Setup.lhs configure
    shell> runhaskell Setup.lhs build
    shell> runhaskell Setup.lhs install
    shell> mkdir ~/.lipl && cp lib/core.lipl ~/.lipl

Or,

    shell> cd src
    shell> ghc --make -O Main.lhs -o lipl
    shell> cp lipl /usr/bin
    shell> mkdir ~/.lipl && cp ../lib/core.lipl ~/.lipl
    shell> lipl
    LIPL> (+ 1 2)
    type: Int
    3
    LIPL> :q
    shell> lipl ../lib/calc.lipl
    ...
    CALC> 1 -1 dup / swap - .
    0
    CALC> :q
    shell>



