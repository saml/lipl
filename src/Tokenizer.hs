module Tokenizer (lex, Token (..)) where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>), (<?>))
import qualified Control.Monad.Error as M

type KeyVal = (String, Token)
type Env = [KeyVal]

data Token = Comment String
    | Ident String
    | Int Integer
    | Float Double
    | Char Char
    | Bool Bool
    | Str String
    | Dict Env
    | List Toks
    | ParenExpr Toks
    deriving (Show)

type Toks = [Token]

data Val = IntVal Integer
    | FloatVal Double
    | CharVal Char
    | BoolVal Bool
    | StrVal String

data Err = ParserErr P.ParseError
    | ArityErr Int Toks
    | Default String

showErr :: Err -> String
showErr (ParserErr err) = "Parse error at " ++ show err
showErr (ArityErr i ts) =
    "Expecting " ++ show i ++ " arguments; found " ++ unwordsToks ts

unwordsToks = unwords . map show

instance Show Err where show = showErr

instance M.Error Err where
    noMsg = Default "Error occurred"
    strMsg = Default

type ThrowErr = Either Err

tokenize :: String -> ThrowErr [Token]
tokenize input = case P.parse parseExpr "lipl" input of
    Left err -> M.throwError $ ParserErr err
    Right val -> return val

parseComment :: P.Parser Token
parseComment = do
    P.char '#'
    s <- P.many (P.noneOf "\n\r")
    return $ Comment s

parseBool :: P.Parser Token
parseBool = do
    b <- P.string "True" <|> P.string "False"
    return $ Bool $ case b of
        "False" -> False
        _ ->  True

identChar :: P.Parser Char
identChar = P.letter <|> P.digit <|> P.oneOf "_-"

parseIdent :: P.Parser Token
parseIdent = parseOp <|> parseName where
    parseOp = do
        op <- P.string "+" <|> P.string "-"
        return $ Ident op
    parseName = do
        first <- P.letter
        rest <- P.many identChar
        return $ Ident (first : rest)

nat = P.many1 P.digit

parseInt :: P.Parser Token
parseInt = do
    sign <- P.try (P.option "" (P.string "-"))
    val <- nat
    return $ Int (read $ sign ++ val)

parseFloat :: P.Parser Token
parseFloat = do
    sign <- P.string "-" <|> return ""
    val <- nat >> P.string "." >> nat
    return $ Float (read $ sign ++ val)

parseChar :: P.Parser Token
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

parseStr :: P.Parser Token
parseStr = do
    P.char '"'
    str <- P.many $ P.many1 (P.noneOf "\"\\") <|> escapedChars
    P.char '"'
    return $ Str (concat str)

lbracket = P.char '[' >> P.spaces
rbracket = P.spaces >> P.char ']'

comma = (P.spaces >> P.char ',' >> P.spaces)
parseList :: P.Parser Token
parseList = do
    lbracket
    l <- P.sepBy parseToken (P.try comma)
    rbracket
    return $ List l

lparen = P.char '(' >> P.spaces
rparen = P.spaces >> P.char ')'

parseParenExpr :: P.Parser Token
parseParenExpr = do
    lparen
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ ParenExpr val

mustSpaces :: P.Parser ()
mustSpaces = P.skipMany1 P.space

lbrace = P.char '{' >> P.spaces
rbrace = P.spaces >> P.char '}'

parseDict :: P.Parser Token
parseDict = do
    lbrace
    keyvals <- P.sepBy parseKeyVal (P.try comma)
    rbrace
    return $ Dict keyvals

parseKeyVal :: P.Parser KeyVal
parseKeyVal = do
    Ident key <- parseIdent
    mustSpaces
    P.char '='
    mustSpaces
    val <- parseToken
    return (key, val)

parseExpr :: P.Parser [Token]
parseExpr = do
    exprs <- P.sepEndBy parseToken mustSpaces
    P.eof
    return exprs

parseToken :: P.Parser Token
parseToken =
    P.try parseComment
    <|> P.try parseList
    <|> P.try parseDict
    <|> P.try parseParenExpr
    <|> P.try parseBool
    <|> P.try parseChar
    <|> P.try parseStr
    <|> P.try parseFloat
    <|> P.try parseInt
    <|> P.try parseIdent

-- primitives = [("+", intOp (+), ]

eval :: Token -> ThrowErr Val
eval (Int i) = return $ IntVal i
eval (Float f) = return $ FloatVal f
eval (Bool b) = return $ BoolVal b
eval (Char c) = return $ CharVal c
eval (Str s) = return $ StrVal s
