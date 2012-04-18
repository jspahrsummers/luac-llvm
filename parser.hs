module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Represents a binary operator in an AST
data BinaryOperator = AddOperator | SubOperator
    deriving Show

-- Represents any expression in an AST
data Expression =
    -- Unary "not" expression
    NotExpression Expression |
    
    -- A literal number value
    NumberLiteral Double |

    -- A binary expression
    BinaryExpression BinaryOperator Expression Expression

    deriving Show

-- Defines all Lua tokens
languageDef = emptyDef {
    Token.reservedOpNames = ["+", "-", "not"]
}

lexer = Token.makeTokenParser languageDef

-- Parses reserved operators in Lua
reservedOp = Token.reservedOp lexer

-- Parses parentheses
parens = Token.parens lexer

-- Parses any integer or floating-point literal
number = Token.naturalOrFloat lexer

-- Parses any spaces
whiteSpace = Token.whiteSpace lexer

-- Defines all Lua operators, their precedence, and their associativity
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

-- Parses expressions
expression :: Parser Expression
expression = buildExpressionParser operators term

-- Parses a Lua string into an expression AST
compileExpression :: String -> Either ParseError Expression
compileExpression str = parse expression "" str
