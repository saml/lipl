module Parser where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import LangData
import CoreLib (builtinNames)
import ParseUtils
import Settings

at val = At initialPos val

parse input = case parseSingle input of
    Right v -> v
    Left err -> error (show err)

parseSingle input = P.parse
    parseSingleExpr (sLANGNAME ++ " REPL") input

parseMultiple fileName input =
    P.parse parseMultipleExpr fileName input

parseBool = do
    pos <- P.getPosition
    b <- P.string "True" <|> P.string "False"
    return (At pos (Bool $ case b of
        "False" -> False
        _ ->  True)) <?> "Bool"

parseIdent = (do
    pos <- P.getPosition
    ident <- parseOp <|> parseName
    return $ At pos (Ident ident)) <?> "Ident"
    where
        parseOp = parseHeadBody opLetter opLetter
        parseName = parseHeadBody identStart identLetter


parsePrimFun = (do
    (At pos (Ident ident)) <- P.try parseIdent
        <|> return (at (Ident ""))
    if ident `elem` builtinNames
        then
            return $ At pos (PrimFun ident)
        else
            fail "") <?> "PrimFun"

parseInt = (do
    pos <- P.getPosition
    sign <- P.try (P.option "" (P.string "-"))
    val <- nat
    return $ At pos (Int (read $ sign ++ val))) <?> "Int"

parseFloat = (do
    pos <- P.getPosition
    sign <- P.string "-" <|> return ""
    n <- nat
    dot <- P.string "."
    frac <- nat
    return $ At pos $ Float (read $ (sign ++ n ++ dot ++ frac)))
    <?> "Float"

parseChar = (do
    pos <- P.getPosition
    P.char '\''
    c <- escapedChar <|> P.anyChar
    --c <- P.letter <|> P.digit <|> P.space <|> escapedChar <|> P.anyChar
    P.char '\''
    return $ At pos $ Char c) <?> "Char"
    where
        escapedChar = do
            s <- escapedChars
            let char = "'" ++ s ++ "'"
            return $ read char

-- TODO: support \32353 and unicode.
escapedChars = do
    P.char '\\' -- get \
    c <- P.anyChar
    return $ case c of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        'v' -> "\v"
        'f' -> "\f"
        'a' -> "\a"
        'b' -> "\b"
        otherwise -> [c]

parseStr = (do
    pos <- P.getPosition
    P.char '"'
    str <- P.many $ P.many1 (P.noneOf "\"\\") <|> escapedChars
    P.char '"'
    return $ At pos $ Str (concat str)) <?> "Str"


parseList = (do
    pos <- P.getPosition
    lbracket
    l <- P.sepBy parseToken (P.try comma)
    rbracket
    return $ At pos $ List l) <?> "List"

getIdents [] = []
getIdents (At _ (Ident a) : xs) = a : getIdents xs
getIdents (Ident a : xs) = a : getIdents xs
getIdents (_ : xs) = getIdents xs

parseParams = (do
    lparen
    params <- P.sepEndBy parseIdent mustSpaces
    rparen
    return (getIdents params)) <?> "Params"

parseLambda = (do
    pos <- P.getPosition
    P.string "lambda"
    mustSpaces
    args <- parseParams
    mustSpaces
    body <- parseToken
    return $ At pos (Lambda args body)) <?> "Lambda"

parseDef = (do
    pos <- P.getPosition
    P.string "def"
    mustSpaces
    (At _ (Ident name)) <- parseIdent
    mustSpaces
    args <- parseParams
    mustSpaces
    body <- parseToken
    return $ At pos (FunDef name args body)) <?> "Def"


parseParenExpr = (do
    pos <- P.getPosition
    lparen
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ At pos (Expr val)) <?> "Paren Expr"

parseIf = (do
    pos <- P.getPosition
    P.string "if"
    mustSpaces
    pred <- parseToken
    mustSpaces
    ifCase <- parseToken
    mustSpaces
    elseCase <- parseToken
    return $ At pos (If pred ifCase elseCase)) <?> "If"

parseLet = (do
    pos <- P.getPosition
    P.string "let"
    mustSpaces
    At _ (Dict env) <- parseDict
    mustSpaces
    body <- parseToken
    return $ At pos (Let env body)) <?> "Let"


parseDict = (do
    pos <- P.getPosition
    lbrace
    l <- P.sepBy parseKeyVal (P.try comma)
    rbrace
    return $ At pos (Dict l)) <?> "Dict"

parseKeyVal = (do
    At _ (Ident key) <- parseIdent
    mustSpaces
    P.char '='
    mustSpaces
    val <- parseToken
    return (key, val)) <?> "KeyVal"

parsePair = (do
    pos <- P.getPosition
    lparen
    a <- parseToken
    comma
    b <- parseToken
    rparen
    return $ At pos (Pair a b)) <?> "Pair"

{-
parseSeq = do
    pos <- P.getPosition
    P.string "seq"
    mustSpaces
    a <- parseToken
    mustSpaces
    b <- parseToken
    return $ At pos (Seq a b)
-}

parseToken =
        P.try parseIf
    <|> P.try parseParenExpr
    <|> P.try parseLet
    <|> P.try parseDef
    <|> P.try parseLambda
    <|> P.try parseList
    <|> P.try parseDict
    <|> P.try parsePair
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
