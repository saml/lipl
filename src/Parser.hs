module Parser where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import qualified Data.Char (isSpace)
import qualified Control.Monad as M

parse :: String -> Either P.ParseError Expr
parse input = P.parse parseExpr "lipl" (trim input)

trim :: String -> String
trim = f . f where
    f = reverse . dropWhile Data.Char.isSpace

type Env a = [(a, E a)]

data E a = Comment String
    | Ident a
    | Int Integer
    | Float Double
    | Bool Bool
    | Char Char
    | Str String
    | List [E a]
--    | Args [a]
    | Body (E a)
    | Def a [a] (E a) -- function name, arguments, body
    | Appl (E a) (E a) -- function, arguments
    | If (E a) (E a) (E a) -- predicate, if-case, else-case
    | Case [(E a, E a)] -- ??
    | Let (Env a) (E a)
    | Expr [E a]
    deriving (Show)



{-
instance (Show a) => Show (E a) where
    show (Ident a) = show a
    show (Int i) = show i
    show (Float f) = show f
    show (Char c) = show c
    show (Str s) = s
    show (List a) = show a
    show (Abst a b) = show a ++ show b
-}

type Expr = E String

{-
showExpr :: Expr -> String
showExpr (Ident a) = a
showExpr (Int a) = show a
showExpr (Float a) = show a
showExpr (Char a) = show a
showExpr (Str a) = a
showExpr (List []) = "[]"
showExpr (List (x:xs)) = show x ++ showExpr (List xs)
showExpr (Abst [] b) = showExpr b
showExpr (Abst a b) = show a ++ showExpr b
showExpr (Appl a b) = showExpr a ++ showExpr b
showExpr (If a b c) = showExpr a ++ showExpr b ++ showExpr c
-}



{-
data Expr = Ident String
    | Int Integer
    | Float Double
    | Char Char
    | Str String
    | List [Expr]
    | Funcall Expr Expr
    | Fundef { ident :: String, args :: [Expr], body :: Expr }
    | If { pred :: Expr, ifCase :: Expr, elseCase :: Expr }
    | Case [(Expr, Expr)]
    | Let { env :: [(Expr, Expr)], body :: Expr }
    | Expr [Expr]
    deriving (Show)
-}


parseComment :: P.Parser Expr
parseComment = do
    P.char '#'
    s <- P.many (P.noneOf "\n\r")
    return $ Comment s

parseBool :: P.Parser Expr
parseBool = do
    b <- P.string "True" <|> P.string "False"
    return $ Bool $ case b of
        "False" -> False
        _ ->  True

identChar :: P.Parser Char
identChar = P.letter <|> P.digit <|> P.oneOf "_-"

parseIdent :: P.Parser Expr
parseIdent = parseOp <|> parseName where
    parseOp = do
        op <- P.string "+" <|> P.string "-"
        return $ Ident op
    parseName = do
        first <- P.letter
        rest <- P.many identChar
        return $ Ident (first : rest)

nat = P.many1 P.digit

parseInt :: P.Parser Expr
parseInt = do
    sign <- P.try (P.option "" (P.string "-"))
    val <- nat
    return $ Int (read $ sign ++ val)

parseFloat :: P.Parser Expr
parseFloat = do
    sign <- P.string "-" <|> return ""
    val <- nat >> P.string "." >> nat
    return $ Float (read $ sign ++ val)

parseChar :: P.Parser Expr
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

parseStr :: P.Parser Expr
parseStr = do
    P.char '"'
    str <- P.many $ P.many1 (P.noneOf "\"\\") <|> escapedChars
    P.char '"'
    return $ Str (concat str)

lbracket = P.char '[' >> P.spaces
rbracket = P.spaces >> P.char ']'

comma :: P.Parser String
comma = do
    P.spaces
    c <- P.char ','
    P.spaces
    return [c]


parseList :: P.Parser Expr
parseList = do
    lbracket
    l <- P.sepEndBy parseToken comma
    rbracket
    return $ List l

parseDef :: P.Parser Expr
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

lparen = P.char '(' >> P.spaces
rparen = P.spaces >> P.char ')'

parseParenExpr :: P.Parser Expr
parseParenExpr = do
    lparen
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ Expr val

{-
parseArgs :: P.Parser Expr
parseArgs = do
    lparen
    args <- P.sepEndBy parseIdent mustSpaces
    rparen
    return $ Args
-}

parseExpr :: P.Parser Expr
parseExpr = do
    val <- P.sepEndBy parseToken mustSpaces
    P.eof
    return $ Expr val

parseToken :: P.Parser Expr
parseToken = -- parseFundef
    P.try parseComment
    <|> P.try parseDef
    <|> P.try parseList
    <|> P.try parseParenExpr
    <|> P.try parseBool
    <|> P.try parseChar
    <|> P.try parseStr
    <|> P.try parseFloat
    <|> P.try parseInt
    <|> P.try parseIdent
--    <|> parseOp

mustSpaces :: P.Parser ()
mustSpaces = P.skipMany1 P.space
-- mustSpaces = P.skipMany1 (comment <|> P.space)

