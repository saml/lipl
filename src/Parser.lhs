==========
Parser.lhs
==========

.. sectnum::
.. contents::

Parser module implements parser of LIPL.

.. sc:: haskell

> module Parser where
>
> import qualified Text.ParserCombinators.Parsec as P
> import Text.ParserCombinators.Parsec ((<|>), (<?>))
>
> import LangData
> import CoreLib (builtinNames)
> import ParseUtils
> import Settings
>
> at val = At initialPos val

at function wraps given Val with initialPos, which is
(line 1, column 1).

.. sc:: haskell

> parse input = case parseSingle input of
>     Right v -> v
>     Left err -> error (show err)
>
> parseSingle input = P.parse
>     parseSingleExpr (sLANGNAME ++ " REPL") input
>
> parseMultiple fileName input =
>     P.parse parseMultipleExpr fileName input

parse function parses String to Val. In case of error,
exception is raised.
parseSingle parses String
that represents a single LIPL expression to Val.
It returns ParseError in case of error.
parseMultiple parses multiple LIPL expressions into Val.
It also returns ParseError in case of error.

.. sc:: haskell

> parseBool = do
>     pos <- P.getPosition
>     b <- P.string "True" <|> P.string "False"
>     return (At pos (Bool $ case b of
>         "False" -> False
>         _ ->  True)) <?> "Bool"

parseBool parses True or False (LIPL booleans).
It retrieves current SourcePos and returns
parsed boolean along with the SourcePos.
Most other parser actions in this module
wraps parsed Val with current SourcePos.

.. sc:: haskell

> parseIdent = (do
>     pos <- P.getPosition
>     ident <- parseOp <|> parseName
>     return $ At pos (Ident ident)) <?> "Ident"
>     where
>         parseOp = parseHeadBody opLetter opLetter
>         parseName = parseHeadBody identStart identLetter

parseIdent parses LIPL identifiers:
apple, app-le, apple' , app''''---__le, +, -,  ...

.. sc:: haskell

> parsePrimFun = (do
>     (At pos (Ident ident)) <- P.try parseIdent
>         <|> return (at (Ident ""))
>     if ident `elem` builtinNames
>         then
>             return $ At pos (PrimFun ident)
>         else
>             fail "") <?> "PrimFun"

parsePrimFun parses one of identifiers defined in builtinNames:
+, -, div, toInt, ...

.. sc:: haskell

> parseInt = (do
>     pos <- P.getPosition
>     sign <- P.try (P.option "" (P.string "-"))
>     val <- nat
>     return $ At pos (Int (read $ sign ++ val))) <?> "Int"
>
> parseFloat = (do
>     pos <- P.getPosition
>     sign <- P.string "-" <|> return ""
>     n <- nat
>     dot <- P.string "."
>     frac <- nat
>     return $ At pos $ Float (read $ (sign ++ n ++ dot ++ frac)))
>     <?> "Float"

parseInt parses LIPL integers: 000, -000024, 235, 00234, ...
parseFloat parses LIPL floats: 00000.000, -0000.003, 00213.23, ...

.. sc:: haskell

> parseChar = (do
>     pos <- P.getPosition
>     P.char '\''
>     c <- escapedChar <|> P.anyChar
>     P.char '\''
>     return $ At pos $ Char c) <?> "Char"
>     where
>         escapedChar = do
>             s <- escapedChars
>             let char = "'" ++ s ++ "'"
>             return $ read char
>
> -- TODO: support \32353 and unicode.
> escapedChars = do
>     P.char '\\' -- get \
>     c <- P.anyChar
>     return $ case c of
>         'n' -> "\n"
>         't' -> "\t"
>         'r' -> "\r"
>         'v' -> "\v"
>         'f' -> "\f"
>         'a' -> "\a"
>         'b' -> "\b"
>         otherwise -> [c]

parseChar parses any single character enclosed in ``' '``
or escaped characters: ``\n``, ``\t``, ...etc.

.. sc:: haskell

> parseStr = (do
>     pos <- P.getPosition
>     P.char '"'
>     str <- P.many $ P.many1 (P.noneOf "\"\\") <|> escapedChars
>     P.char '"'
>     return $ At pos $ Str (concat str)) <?> "Str"

parseStr parses LIPL strings: multi line strings
enclosed in ``" "`` with escaped double quote (``\"``).

.. sc:: haskell

> parseList = (do
>     pos <- P.getPosition
>     lbracket
>     l <- P.sepBy parseToken (P.try comma)
>     rbracket
>     return $ At pos $ List l) <?> "List"

parseList parses LIPL lists::

    [1], [1, "hello"], [  a, b, 35, "asdf"], ...

parseList does not detect heterogeneous lists although LIPL only
allows homogeneous lists.

.. sc:: haskell

> getIdents [] = []
> getIdents (At _ (Ident a) : xs) = a : getIdents xs
> getIdents (Ident a : xs) = a : getIdents xs
> getIdents (_ : xs) = getIdents xs
>
> parseParams = (do
>     lparen
>     params <- P.sepEndBy parseIdent mustSpaces
>     rparen
>     return (getIdents params)) <?> "Params"

getIdents returns identifiers from list of Vals::

    getIdents [Int 1, Ident "a", Ident "b", ...]
    ==> ["a", "b", ...]

parseParams parses parameter list::

    (ident1 ident2 ident3 ... identN)

.. sc:: haskell

> parseLambda = (do
>     pos <- P.getPosition
>     P.string "lambda"
>     mustSpaces
>     args <- parseParams
>     mustSpaces
>     body <- parseToken
>     return $ At pos (Lambda args body)) <?> "Lambda"

parseLambda parses LIPL lambda expression::

    lambda (ident1 ident2 ... identN) expr

.. sc:: haskell

> parseDef = (do
>     pos <- P.getPosition
>     P.string "def"
>     mustSpaces
>     (At _ (Ident name)) <- parseIdent
>     mustSpaces
>     args <- parseParams
>     mustSpaces
>     body <- parseToken
>     return $ At pos (FunDef name args body)) <?> "Def"

parseDef parses LIPL function definition::

    def fname (ident1 ident2 ... identN) expr

.. sc:: haskell

> parseParenExpr = (do
>     pos <- P.getPosition
>     lparen
>     val <- P.sepEndBy parseToken mustSpaces
>     rparen
>     return $ At pos (Expr val)) <?> "Paren Expr"

parseParenExpr parses a LIPL expression, which should be
parenthesized::

    (lambda (x) x), (def f (x) x), (1), (-3.324), ...

.. sc:: haskell

> parseIf = (do
>     pos <- P.getPosition
>     P.string "if"
>     mustSpaces
>     pred <- parseToken
>     mustSpaces
>     ifCase <- parseToken
>     mustSpaces
>     elseCase <- parseToken
>     return $ At pos (If pred ifCase elseCase)) <?> "If"

parseIf parses LIPL if expression::

    (if expr1 expr2 expr3)

.. sc:: haskell

> parseLet = (do
>     pos <- P.getPosition
>     P.string "let"
>     mustSpaces
>     At _ (Dict env) <- parseDict
>     mustSpaces
>     body <- parseToken
>     return $ At pos (Let env body)) <?> "Let"
>
> parseDict = (do
>     pos <- P.getPosition
>     lbrace
>     l <- P.sepBy parseKeyVal (P.try comma)
>     rbrace
>     return $ At pos (Dict l)) <?> "Dict"
>
> parseKeyVal = (do
>     At _ (Ident key) <- parseIdent
>     mustSpaces
>     P.char '='
>     mustSpaces
>     val <- parseToken
>     return (key, val)) <?> "KeyVal"

parseLet parses LIPL let expression::

    (let { i1 = expr1, i2 = expr2, ... iN = exprN }
         expr)

parseKeyVal parses::

    identifier = expr

parseDict parses::

    { identifer1 = expr1 , identifier2 = expr2
        , ..., identiferN = exprN}

.. sc:: haskell

> parsePair = (do
>     pos <- P.getPosition
>     lparen
>     a <- parseToken
>     comma
>     b <- parseToken
>     rparen
>     return $ At pos (Pair a b)) <?> "Pair"

parsePair parses::

    (expr1, expr2)

.. sc:: haskell

> parseToken =
>         P.try parseIf
>     <|> P.try parseParenExpr
>     <|> P.try parseLet
>     <|> P.try parseDef
>     <|> P.try parseLambda
>     <|> P.try parseList
>     <|> P.try parseDict
>     <|> P.try parsePair
>     <|> P.try parseBool
>     <|> P.try parseChar
>     <|> P.try parseStr
>     <|> P.try parseFloat
>     <|> P.try parseInt
>     <|> P.try parsePrimFun
>     <|> P.try parseIdent

parseToken parses a LIPL token, which can be a nested
parenthesized expression.

.. sc:: haskell

> parseSingleExpr = do
>     ws
>     e <- parseToken
>     ws
>     P.eof
>     return e
>
> parseMultipleExpr = do
>     ws
>     es <- P.sepEndBy parseParenExpr ws
>     P.eof
>     return es

parseSingleExpr parses a LIPL token (includes parenthesised
expression).
parseMultipleExpr parses multiple parenthesized expressions.

