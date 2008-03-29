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

lexer  = P.makeTokenParser (P.haskellStyle {
    P.commentLine = "#"
    , P.identLetter = P.alphaNum <|> P.oneOf "_'-"
--    , P.reservedNames = ["def", "if", "let", "lambda"]
    })

ws = P.whiteSpace lexer
mustSpaces = P.skipMany1 P.space >> ws

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

identChar = P.letter <|> P.digit <|> P.oneOf "_-"
opChar = P.oneOf ":!$%&*+./<=>?@\\^|-~"

parseHeadBody headChar bodyChar = do
    h <- headChar
    b <- P.many bodyChar
    return (h : b)

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

nat = P.many1 P.digit

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

lbracket = P.char '[' >> P.spaces
rbracket = P.spaces >> P.char ']'

comma = (P.spaces >> P.char ',' >> P.spaces)

parseList = do
    lbracket
    l <- P.sepBy parseToken (P.try comma)
    rbracket
    return $ List l

getIdents [] = []
getIdents (Ident a:xs) = a : getIdents xs
getIdents (_:xs) = getIdents xs

parseLambda = do
    P.string "lambda"
    mustSpaces
    Expr args <- parseParenExpr
    mustSpaces
    body <- parseToken
    return $ Lambda (getIdents args) body

parseDef = do
    P.string "def"
    mustSpaces
    Ident name <- parseIdent
    mustSpaces
    Expr args <- parseParenExpr
    mustSpaces
    body <- parseToken
    return $ FunDef name (getIdents args) body

lparen = P.char '(' >> P.spaces
rparen = P.spaces >> P.char ')'

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

lbrace = P.char '{' >> P.spaces
rbrace = P.spaces >> P.char '}'

parseDict = do
    lbrace
    l <- P.sepBy parseKeyVal (P.try comma)
    rbrace
    return $ Dict (Map.fromList l)

parseKeyVal = do
    Ident key <- parseIdent
    mustSpaces
    P.char '='
    mustSpaces
    val <- parseToken
    return (key, val)


parseToken =
        P.try parseIf
    <|> P.try parseLet
    <|> P.try parseDef
    <|> P.try parseLambda
    <|> P.try parseList
    <|> P.try parseDict
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
