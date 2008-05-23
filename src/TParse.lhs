==========
TParse.lhs
==========

.. sectnum::
.. contents::

TParse provides functions for parsing type expressions.
Although LIPL does not allow type annotations,
using type parsing functions, one can write type expressions like::

    a -> b

    instead of

    TApp (TApp (TConst "->") TVar "a") (TVar "b")

.. sc:: haskell

> module TParse where
>
> import qualified Text.ParserCombinators.Parsec as P
> import qualified Text.ParserCombinators.Parsec.Token as P
> import qualified Text.ParserCombinators.Parsec.Expr as P
> import qualified Text.ParserCombinators.Parsec.Language as P
> import Text.ParserCombinators.Parsec ((<|>), (<?>))
>
> import ParseUtils hiding (lexer)
> import Type
>
> tParse texpr = case P.parse parseExpr "type expression" texpr of
>     Right t -> t
>     Left err -> error (show err ++ ": type expression parse error")

tParse parses a type expression to Type.
In case of error, exception is raised (program quits).

.. sc:: haskell

> lexer  = P.makeTokenParser (P.haskellStyle {
>     P.reservedOpNames = ["->"]
>     })
>
> parens = P.parens lexer
> reservedOp = P.reservedOp lexer
> lexeme = P.lexeme lexer
> term = lexeme parseType <|> parens expr <?> "term"

parens takes a parser and creates a parser that
parses what the given parser parses
enclosed in parenthesis::

    parens p
    ==> parses ( what p parses )

reservedOp takes a String and creates a parser that
accepts input that is same as the given String.
The given String can be the reserved operator (``->``).

lexeme takes a parser and creates a parser that
accepts what the given parser accepts and skips whitespaces
after parsing.

term parses what parseType parses or parenthesized expr.

.. sc:: haskell

> expr = P.buildExpressionParser table term <?> "expr"
>
> table = [
>     [P.Infix (do
>         reservedOp "->"
>         return fn
>         <?> "operator") P.AssocRight]]

expr parses terms connected with ``->``::

    term1 -> term2 -> term3 ...

.. sc:: haskell

> parseExpr = do
>     t <- expr
>     P.eof
>     return t <?> "Expression"

parseExpr parses a type expression.

Below parsers are straight forward:

.. sc:: haskell

> parseTVar = do
>     x <- P.letter
>     xs <- P.many P.alphaNum
>     return (TVar (x:xs)) <?> "Variable"
>
> parseFunc = do
>     t1 <- parseType
>     P.spaces
>     P.string "->"
>     P.spaces
>     t2 <- parseType
>     return (t1 `fn` t2) <?> "Function"
>
> parseInt = do
>     P.string "Int"
>     return tInt <?> "Int"
>
> parseFloat = do
>     P.string "Float"
>     return tFloat <?> "Float"
>
> parseBool = do
>     P.string "Bool"
>     return tBool <?> "Bool"
>
> parseChar = do
>     P.string "Char" <?> "Char"
>     return tChar
>
> parseStr = do
>     P.string "Str" <?> "Str"
>     return $ list tChar
>
> parseList = do
>     lbracket
>     t <- term
>     rbracket
>     return (list t) <?> "List"
>
> parseUnit = do
>     lparen
>     rparen
>     return tUnit <?> "Unit"
>
> parsePair = do
>     lparen
>     t1 <- parseType
>     comma
>     t2 <- parseType
>     rparen
>     return (pair t1 t2) <?> "Pair"
>
> parseType = do
>     P.try parseList
>     <|> P.try parseInt
>     <|> P.try parseFloat
>     <|> P.try parseBool
>     <|> P.try parseChar
>     <|> P.try parseStr
>     <|> P.try parsePair
>     <|> P.try parseUnit
>     <|> P.try parseTVar

parseType parses any valid Type::

    Int, Bool, [((Int -> a) -> b -> Char)], [Char] -> a, ...

Note that parenthesis are needed in ``[(a -> b)]`` because
parseList expects a term inside ``[ ]``.
``[a -> b]`` won't get parsed.
