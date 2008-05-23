==============
ParseUtils.lhs
==============

.. sectnum::
.. contents::

ParseUtils provide useful functions that can be used in Parser.
Detailed documentation about Parsec library, that is used
in this module, can be found at:
http://research.microsoft.com/users/daan/parsec.html

.. sc:: lhs

> module ParseUtils where
>
> import qualified Text.ParserCombinators.Parsec as P
> import qualified Text.ParserCombinators.Parsec.Token as P
> import qualified Text.ParserCombinators.Parsec.Expr as P
> import qualified Text.ParserCombinators.Parsec.Language as P
> import Text.ParserCombinators.Parsec ((<|>))
>
> liplStyle = P.haskellStyle {
>     P.commentLine = "#"
>     , P.identLetter = P.alphaNum <|> P.oneOf "_'-"
>     }

liplStyle is a LanguageDef.
It is based on haskellStyle but
defines line comment to start with ``#``.
And, identifers can include ``-`` (in Haskell, identifiers can't include
``-``).
Since liplStyle inherites most of the settings from haskellStyle,
nested block comments (``{- -}``) are already defined.

.. sc:: lhs

> lexer  = P.makeTokenParser liplStyle

When a parser (CharParser, GenParser...etc)
is built from above lexer (which is a TokenParser),
the parser will get proper tokens:
line comments starting with ``#`` are discarded (so are
nested block comments inside ``{- -}``), identifiers
are tokenized so that they can contain alphaNum or one of
``_'-``...etc.

.. sc:: lhs

> ws = P.whiteSpace lexer
> mustSpaces = P.skipMany1 P.space >> ws

ws parses zero or more white spaces.
musteSpace parses one or more white spaces.

.. sc:: lhs

> identStart = P.identStart liplStyle
> identLetter = P.identLetter liplStyle
> opLetter = P.opLetter liplStyle

identStart parses 1 character that can start an identifier (alphabet
character).
identLetter parses 1 character that can be in an identifier
(alphaNum, one of ``_-'``).
opLetter parses 1 character that can be in an operator
(``:!+./<=>?``...etc)

.. sc:: lhs

> parseHeadBody headChar bodyChar = do
>     h <- headChar
>     b <- P.many bodyChar
>     return (h : b)

Given 2 character parsers, parseHeadBody parses
a string that starts with one of headChar and continues with
one of bodyChar.

.. sc:: lhs

> nat = P.many1 P.digit
>
> lbracket = P.char '[' >> P.spaces
> rbracket = P.spaces >> P.char ']'
>
> comma = (P.spaces >> P.char ',' >> P.spaces)
>
> lparen = P.char '(' >> P.spaces
> rparen = P.spaces >> P.char ')'
>
> lbrace = P.char '{' >> P.spaces
> rbrace = P.spaces >> P.char '}'

nat parses 1 or more digits.
lbracket, rbracket, comma, lparen, rparen, lbrace, and rbrace
parses ``[],(){}`` respectively with optional white spaces
around them.
