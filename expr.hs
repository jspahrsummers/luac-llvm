import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data BinaryOperator = AddOperator | SubOperator
    deriving Show

data Expression =
    NotExpression Expression |
    NumberLiteral Double |
    BinaryExpression BinaryOperator Expression Expression
    deriving Show

languageDef = emptyDef {
    Token.reservedOpNames = ["+", "-", "not"]
}

lexer = Token.makeTokenParser languageDef
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
number = Token.naturalOrFloat lexer
whiteSpace = Token.whiteSpace lexer

operators = [
    [Prefix (reservedOp "not" >> return (NotExpression))],
    [Infix (reservedOp "+" >> return (BinaryExpression AddOperator)) AssocLeft],
    [Infix (reservedOp "-" >> return (BinaryExpression SubOperator)) AssocLeft]]

doubleFromEither :: Either Integer Double -> Double
doubleFromEither (Right x) = x
doubleFromEither (Left x) = fromInteger x

term =
    parens expression <|>
    liftM (NumberLiteral . doubleFromEither) number

expression :: Parser Expression
expression = buildExpressionParser operators term

compile :: String -> Either ParseError Expression
compile str = parse expression "" str
