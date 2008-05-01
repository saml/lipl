module Parser where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
--import qualified Text.ParserCombinators.Parsec.Expr as P
import qualified Text.ParserCombinators.Parsec.Language as P
import Text.ParserCombinators.Parsec ((<|>))

import qualified Control.Monad.Error as E
import qualified Data.Map as Map
import Data.Char ( chr )

import LangData
import CoreLib (builtinNames)
import ParseUtils
import Utils

{-
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural = P.natural lexer
parens = P.parens lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

runLex p input = P.parse (do
    x <- p
    P.eof
    return x) "" input
-}

parseSingle input = P.parse parseSingleExpr "lipl" input

parseMultiple fileName input =
    P.parse parseMultipleExpr fileName input

parseComment = do
    P.char '#'
    s <- P.many (P.noneOf "\n")
    return ()

parseBool = do
    b <- P.string "True" <|> P.string "False"
    return $ Bool $ case b of
        "False" -> False
        _ ->  True

parseIdent = do
    ident <- parseOp <|> parseName
    return $ Ident ident
    where
        parseOp = parseHeadBody opChar opChar
        parseName = parseHeadBody identChar identChar

parsePrimFun = do
    (Ident ident) <- P.try parseIdent <|> return (Ident "")
    if ident `elem` builtinNames
        then
            return $ PrimFun ident
        else
            fail ""


parseInt = do
    sign <- P.try (P.option "" (P.string "-"))
    val <- nat
    return $ Int (read $ sign ++ val)

parseFloat = do
    sign <- P.string "-" <|> return ""
    n <- nat
    dot <- P.string "."
    frac <- nat
    return $ Float (read $ (sign ++ n ++ dot ++ frac))

parseChar = do
    P.char '\''
    c <- P.letter <|> P.digit <|> P.space <|> escapedChar
    P.char '\''
    return $ Char c
    where
        escapedChar = do
            s <- escapedChars
            let char = "'" ++ s ++ "'"
            return $ read char

-- TODO: support \32353 and unicode.
escapedChars = do
    P.char '\\' -- get \
    --c <- P.oneOf "\\\"ntrvf" -- \, ", n, t, r
    c <- P.anyChar
    return $ case c of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        'v' -> "\v"
        'f' -> "\f"
        'a' -> "\a"
        'b' -> "\b"
        --'v' -> [chr 11]
        --'f' -> [chr 12]
        --'a' -> [chr 7]
        --'b' -> [chr 8]
        otherwise -> [c]

parseStr = do
    P.char '"'
    str <- P.many $ P.many1 (P.noneOf "\"\\") <|> escapedChars
    P.char '"'
    return $ Str (concat str)


parseList = do
    lbracket
    l <- P.sepBy parseToken (P.try comma)
    rbracket
    return $ List l

getIdents [] = []
getIdents (Ident a:xs) = a : getIdents xs
getIdents (_:xs) = getIdents xs

getParams l = do
    let params = getIdents l
    if noDup params
        then
            return params
        else
            fail "Parameters can't have duplicates"

parseLambda = do
    P.string "lambda"
    mustSpaces
    Expr args <- parseParenExpr
    mustSpaces
    body <- parseToken
    --params <- getParams args
    let params = getIdents args
    return $ Lambda params body

{-
    let params = getIdents args
    if noDup params
        then
            return $ Lambda params body
        else
            fail "Parameters can't be same"
-}

parseDef = do
    P.string "def"
    mustSpaces
    Ident name <- parseIdent
    mustSpaces
    Expr args <- parseParenExpr
    mustSpaces
    body <- parseToken
    return $ FunDef name (getIdents args) body


parseParenExpr = do
    lparen
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ Expr val

parseIf = do
    P.string "if"
    mustSpaces
    pred <- parseToken
    mustSpaces
    ifCase <- parseToken
    mustSpaces
    elseCase <- parseToken
    return $ If pred ifCase elseCase

parseLet = do
    P.string "let"
    mustSpaces
    Dict env <- parseDict
    mustSpaces
    body <- parseToken
    return $ Let env body


parseDict = do
    lbrace
    l <- P.sepBy parseKeyVal (P.try comma)
    rbrace
    return $ Dict l

parseKeyVal = do
    Ident key <- parseIdent
    mustSpaces
    P.char '='
    mustSpaces
    val <- parseToken
    return (key, val)

parsePair = do
    lparen
    a <- parseToken
    comma
    b <- parseToken
    rparen
    return $ Pair a b

parseToken =
        P.try parseIf
    <|> P.try parseLet
    <|> P.try parseDef
    <|> P.try parseLambda
    <|> P.try parseList
    <|> P.try parseDict
    <|> P.try parsePair
    <|> P.try parseParenExpr
    <|> P.try parseBool
    <|> P.try parseChar
    <|> P.try parseStr
    <|> P.try parseFloat
    <|> P.try parseInt
    <|> P.try parsePrimFun
    <|> P.try parseIdent

parseSingleExpr = do
    ws
    e <- parseToken
    ws
    P.eof
    return e

parseMultipleExpr = do
    ws
    es <- P.sepEndBy parseParenExpr ws
    P.eof
    return es
