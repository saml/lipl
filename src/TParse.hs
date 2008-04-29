module TParse where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Expr as P
import qualified Text.ParserCombinators.Parsec.Language as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import ParseUtils hiding (lexer)
import Type

lexer  = P.makeTokenParser (P.haskellStyle {
    P.reservedOpNames = ["->"]
    })
parens = P.parens lexer
reservedOp = P.reservedOp lexer
lexeme = P.lexeme lexer

tParse texpr = case P.parse parseExpr "type expression" texpr of
    Right t -> t
    Left err -> error (show err ++ ": type expression parse error")

term = lexeme parseType <|> parens expr <?> "term"

expr = P.buildExpressionParser table term <?> "expr"

table = [
    [P.Infix (do
        reservedOp "->"
        return fn
        <?> "operator") P.AssocRight]]


parseExpr = do
    t <- expr
    P.eof
    return t <?> "Expression"

parseTVar = do
    x <- P.letter
    xs <- P.many P.alphaNum
    return (TVar (x:xs)) <?> "Variable"

{-
parseTApp = do
    t1 <- parseType
    P.spaces
    t2 <- parseType
    return (TApp t1 t2)
-}

parseArrow = do
    P.string "->"
    return fn

parseFunc = do
    t1 <- parseType
    P.spaces
    P.string "->"
    P.spaces
    t2 <- parseType
    return (t1 `fn` t2) <?> "Arrow"

{-
parseManyTVar = do
    tVars <- P.many parseTVar
    return tVars
-}

{-
parseTConst = do
    P.try (P.string "()" >> return tUnit)
    <|> P.try (P.string "Int" >> return tInt)
    <|> P.try (P.string "Float" >> return tFloat)
    <|> P.try (P.string "Bool" >> return tBool)
    <|> P.try (P.string "Char" >> return tChar)
-}

parseInt = do
    P.string "Int"
    return tInt <?> "Int"

parseFloat = do
    P.string "Float"
    return tFloat <?> "Float"

parseBool = do
    P.string "Bool"
    return tBool <?> "Bool"

parseChar = do
    P.string "Char" <?> "Char"
    return tChar

parseStr = do
    P.string "Str" <?> "Str"
    return $ list tChar

parseList = do
    lbracket
    t <- term
    rbracket
    return (list t) <?> "List"

parseUnit = do
    lparen
    rparen
    return tUnit <?> "Unit"

parseType = do
    P.try parseList
    <|> P.try parseInt
    <|> P.try parseFloat
    <|> P.try parseBool
    <|> P.try parseChar
    <|> P.try parseStr
    <|> P.try parseUnit
    <|> P.try parseTVar
    -- <|> P.try parseArrow

{-
expr = P.buildExpressionParser table el
table = [
    [P.op
    ]
-}
