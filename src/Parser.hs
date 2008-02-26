module Parser where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import qualified Data.Char (isSpace)

parse :: String -> Either P.ParseError Expr
parse input = P.parse parseExpr "lipl" (trim input)

trim :: String -> String
trim = f . f where
    f = reverse . dropWhile Data.Char.isSpace

type Env a = [(a, E a)]

data E a = Ident a
    | Int Integer
    | Float Double
    | Char Char
    | Str String
    | List [E a]
    | Abst [a] (E a) -- arguments, body
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

parseInt :: P.Parser Expr
parseInt = do
    sign <- P.try (P.option "" (P.string "-"))
    val <- P.many1 P.digit
    return $ Int (read $ sign ++ val)

{-
parseFundef :: P.Parser Expr
parseFundef = do
    P.string "def"
    mustSpaces
    Ident name <- parseIdent
    mustSpaces
    ParenExpr args <- parseParenExpr
    mustSpaces
    Expr (body:_) <- parseExpr
    return $ Fundef name args body
-}

parseStr :: P.Parser Expr
parseStr = do
    P.char '"'
    str <- P.many $ P.many1 (P.noneOf "\"\\") <|> escapedChars
    P.char '"'
    return $ Str (concat str)
    where
        escapedChars = do
            P.char '\\' -- back slash
            c <- P.oneOf "\\\"" -- \\ \"
            return [c]

lparen = P.char '(' >> P.spaces

rparen = P.spaces >> P.char ')'

parseParenExpr :: P.Parser Expr
parseParenExpr = do
    lparen
    val <- P.sepEndBy parseToken mustSpaces
    rparen
    return $ Expr val

parseExpr :: P.Parser Expr
parseExpr = do
    val <- P.sepEndBy parseToken mustSpaces
    P.eof
    return $ Expr val

parseToken :: P.Parser Expr
parseToken = -- parseFundef
    P.try parseParenExpr
    <|> P.try parseStr
    <|> P.try parseInt
    <|> P.try parseIdent
--    <|> parseOp

mustSpaces :: P.Parser ()
mustSpaces = P.skipMany1 P.space

