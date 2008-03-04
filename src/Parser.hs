module Parser where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
--import qualified Data.Char (isSpace)
--import qualified Control.Monad as M
--import qualified Control.Monad.Error as M

import LangData

parse :: String -> Either P.ParseError Val
parse input = P.parse parseExpr "lipl" input

--trim :: String -> String
--trim = f . f where
--    f = reverse . dropWhile Data.Char.isSpace

{-
type KeyVal a = (a, E a)
type Env a = [KeyVal a]

-- | a is for id for bound vars: String or Int...etc
data E a = Comment String
    | Ident a
    | Int Integer
    | Float Double
    | Bool Bool
    | Char Char
    | Str String
    | List [E a]
--    | Args [a]
--    | Body (E a)
    | Def { funName :: a, funArgs :: [a], funBody :: (E a) }
    | Appl { funExpr :: (E a), argsExpr :: (E a) }
    | If { pred :: (E a), ifCase :: (E a), elseCase :: (E a) }
    | Case [(E a, E a)] -- ??
    | Let { letEnv :: (Env a), letBody :: (E a) }
    | Expr [E a]
    | TopLevel [E a]
    deriving (Show)

type Expr = E String
-}


{-
instance (Show a) => Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Ident a) = a
showExpr (Int a) = show a
showExpr (Float a) = show a
showExpr (Char a) = show a
showExpr (Bool a) = show a
showExpr (Str a) = a
showExpr (List []) = "[]"
showExpr (List (x:xs)) = show x ++ showExpr (List xs)
showExpr (Def fn args b) =
    "<function: " ++ fn ++ " " ++ showExpr (List args) ++ ">"
showExpr (If p i e) =
    "if " ++ showExpr p ++ " then "
        ++ showExpr i ++ " else " ++ showExpr e
showExpr (Let d b) = "let " ++ showExpr d ++ " in " ++ showExpr b
-}

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
    c <- P.oneOf "\\\"ntr" -- \, ", n, t, r
    return $ case c of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        _ -> [c]

parseStr = do
    P.char '"'
    str <- P.many $ P.many1 (P.noneOf "\"\\") <|> escapedChars
    P.char '"'
    return $ Str (concat str)

--lbracket = P.symbol "[" --P.char '[' >> P.spaces
--rbracket = P.symbol "]" --P.spaces >> P.char ']'
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
    return l

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
