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
import Settings
import PosMonadClass

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

parse input = case parseSingle input of
    Right v -> v
    Left err -> error (show err)

parseSingle input = P.parse parseSingleExpr (sLANGNAME ++ " REPL") input

parseMultiple fileName input =
    P.parse parseMultipleExpr fileName input

parseComment = do
    P.char '#'
    s <- P.many (P.noneOf "\n")
    return ()

parseBool = do
    pos <- P.getPosition
    b <- P.string "True" <|> P.string "False"
    return $ At pos (Bool $ case b of
        "False" -> False
        _ ->  True)

parseIdent = do
    pos <- P.getPosition
    ident <- parseOp <|> parseName
    return $ At pos (Ident ident)
    where
        parseOp = parseHeadBody opChar opChar
        parseName = parseHeadBody identChar identChar

at val = At initialPos val

parsePrimFun = do
    (At pos (Ident ident)) <- P.try parseIdent <|> return (at (Ident ""))
    if ident `elem` builtinNames
        then
            return $ At pos (PrimFun ident)
        else
            fail ""

parseInt = do
    pos <- P.getPosition
    sign <- P.try (P.option "" (P.string "-"))
    val <- nat
    return $ At pos (Int (read $ sign ++ val))

parseFloat = do
    pos <- P.getPosition
    sign <- P.string "-" <|> return ""
    n <- nat
    dot <- P.string "."
    frac <- nat
    return $ At pos $ Float (read $ (sign ++ n ++ dot ++ frac))

parseChar = do
    pos <- P.getPosition
    P.char '\''
    c <- P.letter <|> P.digit <|> P.space <|> escapedChar
    P.char '\''
    return $ At pos $ Char c
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
    pos <- P.getPosition
    P.char '"'
    str <- P.many $ P.many1 (P.noneOf "\"\\") <|> escapedChars
    P.char '"'
    return $ At pos $ Str (concat str)


parseList = do
    pos <- P.getPosition
    lbracket
    l <- P.sepBy parseToken (P.try comma)
    rbracket
    return $ At pos $ List l

getIdents [] = []
getIdents (At _ (Ident a) : xs) = a : getIdents xs
getIdents (Ident a : xs) = a : getIdents xs
getIdents (_ : xs) = getIdents xs

parseParams = do
    lparen
    params <- P.sepEndBy parseIdent mustSpaces
    rparen
    return (getIdents params)
{-
    where
        extractIds [] = []
        extractIds (At _ (Ident x) : xs) = x : extractIds xs
        extractIds (Ident x : xs) = x : extractIds xs
        extractIds (_ : xs) = extractIds xs
-}

getParams l = do
    let params = getIdents l
    if noDup params
        then
            return params
        else
            fail "Parameters can't have duplicates"

parseLambda = do
    pos <- P.getPosition
    P.string "lambda"
    mustSpaces
    args <- parseParams
    mustSpaces
    body <- parseToken
    --let params = getIdents args
    return $ At pos (Lambda args body)

{-
    let params = getIdents args
    if noDup params
        then
            return $ Lambda params body
        else
            fail "Parameters can't be same"
-}

parseDef = do
    pos <- P.getPosition
    P.string "def"
    mustSpaces
    (At _ (Ident name)) <- parseIdent
    mustSpaces
    args <- parseParams
    mustSpaces
    body <- parseToken
    return $ At pos (FunDef name args body)


parseParenExpr = do
    pos <- P.getPosition
    lparen
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ At pos (Expr val)

parseIf = do
    pos <- P.getPosition
    P.string "if"
    mustSpaces
    pred <- parseToken
    mustSpaces
    ifCase <- parseToken
    mustSpaces
    elseCase <- parseToken
    return $ At pos (If pred ifCase elseCase)

parseLet = do
    pos <- P.getPosition
    P.string "let"
    mustSpaces
    At _ (Dict env) <- parseDict
    mustSpaces
    body <- parseToken
    return $ At pos (Let env body)


parseDict = do
    pos <- P.getPosition
    lbrace
    l <- P.sepBy parseKeyVal (P.try comma)
    rbrace
    return $ At pos (Dict l)

parseKeyVal = do
    At _ (Ident key) <- parseIdent
    mustSpaces
    P.char '='
    mustSpaces
    val <- parseToken
    return (key, val)

parsePair = do
    pos <- P.getPosition
    lparen
    a <- parseToken
    comma
    b <- parseToken
    rparen
    return $ At pos (Pair a b)

parseSeq = do
    pos <- P.getPosition
    P.string "seq"
    mustSpaces
    a <- parseToken
    mustSpaces
    b <- parseToken
    return $ At pos (Seq a b)

parseToken =
        P.try parseIf
    <|> P.try parseSeq
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
