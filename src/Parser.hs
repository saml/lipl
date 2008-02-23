module Parser where

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import Data.Char (isSpace)

parse :: String -> Either P.ParseError Expr
parse input = P.parse parseExpr "lipl" (trim input)

trim :: String -> String
trim = f . f where
    f = reverse . dropWhile isSpace

type Name = String
data Expr = Ident Name
--    | Op Name
    | Int Integer
    | Float Double
    | Char Char
    | Str String
    | List [Expr]
--    | Funcall { name :: Name, args :: [Expr] }
--    | Fundef { name :: Name, args :: [Expr], body :: Expr }
--    | ParenExpr [Expr]
    | Expr [Expr]
    deriving (Show)

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

{-
parseOp :: P.Parser Expr
parseOp = do
    val <- P.string "="
        <|> P.string "+" <|> P.string "-"
    return $ Op val
-}

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
    <|> P.try parseInt
    <|> P.try parseIdent
--    <|> parseOp

mustSpaces :: P.Parser ()
mustSpaces = P.skipMany1 P.space

