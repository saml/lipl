module Parser where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import qualified Control.Monad.Error as E

import Data.Char ( chr )
import LangData

parseSingle :: String -> CanBeErr Val
parseSingle input = case P.parse parseSingleExpr "lipl" input of
    Left err -> E.throwError $ ParseErr err
    Right val -> return val

parse :: String -> CanBeErr Val
parse input = case P.parse parseExpr "lipl" input of
    Left err -> E.throwError $ ParseErr err
    Right val -> return val

parseComment = do
    P.char '#'
    s <- P.many (P.noneOf "\n\r")
    return $ Comment s

parseBool = do
    b <- P.string "True" <|> P.string "False"
    return $ Bool $ case b of
        "False" -> False
        _ ->  True

identChar = P.letter <|> P.digit <|> P.oneOf "_-"
opChar = P.oneOf ":!#$%&*+./<=>?@\\^|-~"

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

{-
parseDef = do
    P.string "def"
    mustSpaces
    Ident name <- parseIdent
    mustSpaces
    Expr args <- parseParenExpr
    mustSpaces
    body <- parseToken
    return $ Def name (getIdents args) body
    where
        getIdents [] = []
        getIdents (Ident a:xs) = [a] ++ getIdents xs
--        getIdents _ = error "Blah" -- TODO: better error handling
-}

lparen = P.char '(' >> P.spaces
rparen = P.spaces >> P.char ')'

parseParenExpr = do
    lparen
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ Expr val

{-
parseIf :: P.Parser Expr
parseIf = do
    P.string "if"
    mustSpaces
    pred <- parseToken
    mustSpaces
    ifCase <- parseToken
    mustSpaces
    elseCase <- parseToken
    return $ If pred ifCase elseCase

parseLet :: P.Parser Expr
parseLet = do
    P.string "let"
    mustSpaces
    env <- parseDict
    mustSpaces
    body <- parseToken
    return $ Let env body
-}

lbrace = P.char '{' >> P.spaces
rbrace = P.spaces >> P.char '}'

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

mustSpaces = P.skipMany1 P.space

{-
parseTopLevel = do
    exprs <- P.sepEndBy parseParenExpr mustSpaces
    P.eof
    return $ TopLevel exprs
-}

parseToken =
    P.try parseComment
--    <|> P.try parseIf
--    <|> P.try parseLet
--    <|> P.try parseDef
    <|> P.try parseList
    <|> P.try parseDict
    <|> P.try parseParenExpr
    <|> P.try parseBool
    <|> P.try parseChar
    <|> P.try parseStr
    <|> P.try parseFloat
    <|> P.try parseInt
    <|> P.try parseIdent

parseExpr = do
    toks <- P.sepEndBy parseToken mustSpaces
    return $ Expr toks

parseSingleExpr = do
    e <- parseExpr
    P.eof
    return e
