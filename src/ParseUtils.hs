module ParseUtils where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Expr as P
import qualified Text.ParserCombinators.Parsec.Language as P
import Text.ParserCombinators.Parsec ((<|>))

lexer  = P.makeTokenParser (P.haskellStyle {
    P.commentLine = "#"
    , P.identLetter = P.alphaNum <|> P.oneOf "_'-"
--    , P.reservedNames = ["def", "if", "let", "lambda"]
    })

ws = P.whiteSpace lexer
mustSpaces = P.skipMany1 P.space >> ws

identChar = P.letter <|> P.digit <|> P.oneOf "_-"
opChar = P.oneOf ":!$%&*+./<=>?@\\^|-~"

parseHeadBody headChar bodyChar = do
    h <- headChar
    b <- P.many bodyChar
    return (h : b)

nat = P.many1 P.digit

lbracket = P.char '[' >> P.spaces
rbracket = P.spaces >> P.char ']'

comma = (P.spaces >> P.char ',' >> P.spaces)

lparen = P.char '(' >> P.spaces
rparen = P.spaces >> P.char ')'

lbrace = P.char '{' >> P.spaces
rbrace = P.spaces >> P.char '}'
