module TParse where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Expr as P
import qualified Text.ParserCombinators.Parsec.Language as P
import Text.ParserCombinators.Parsec ((<|>))

import Type

tParse texpr = P.parse parseType "type expression" texpr

parseTVar = do
    x <- P.letter
    xs <- P.many P.alphaNum
    return $ TVar (x:xs)

parseType = do
    P.try parseTVar

lexer  = P.makeTokenParser (P.haskellStyle {
    P.commentLine = "#"
    , P.identLetter = P.alphaNum <|> P.oneOf "_'-"
--    , P.reservedNames = ["def", "if", "let", "lambda"]
    })

{-
expr = P.buildExpressionParser table el
table = [
    [P.op
    ]
-}
ws = P.whiteSpace lexer
mustSpaces = P.skipMany1 P.space >> ws


