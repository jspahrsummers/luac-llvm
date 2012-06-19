{- Contains functions for parsing Lua into an AST -}
module Parser where

import AST
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Language definition parameters specific to Lua
languageDef = emptyDef {
    Token.reservedOpNames = ["+", "-", "not"],
    Token.reservedNames = ["true", "false", "nil"],
    Token.identStart = letter <|> char '_',
    Token.identLetter = alphaNum <|> char '_'
}

lexer = Token.makeTokenParser languageDef

-- Defines lexers that can be applied to an input stream
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
identifier = Token.identifier lexer
number = Token.naturalOrFloat lexer
whiteSpace = Token.whiteSpace lexer
commaSep = Token.commaSep lexer
string = Token.stringLiteral lexer

-- Defines all Lua operators, their precedence, and their associativity
operators = [
    [Prefix (reservedOp "not" >> return (NotExpression))],
    [Infix (reservedOp "+" >> return (BinaryExpression AddOperator)) AssocLeft],
    [Infix (reservedOp "-" >> return (BinaryExpression SubOperator)) AssocLeft]]

-- Returns a double from the Either parsed by the "number" lexer
doubleFromEither :: Either Integer Double -> Double
doubleFromEither (Right x) = x
doubleFromEither (Left x) = fromInteger x

-- Parsers for nonterminals
prefixexp =
    parens Parser.exp <|>
    functioncall <|>
    liftM (NumberLiteral . doubleFromEither) number <|>
    liftM StringLiteral Parser.string <|>
    nil <|> true <|> false

nil = do
    reserved "nil"
    return NilLiteral

false = do
    reserved "false"
    return $ BooleanLiteral False

true = do
    reserved "true"
    return $ BooleanLiteral True

functioncall = do
    var <- identifier
    spaces
    args <- parens (commaSep Parser.exp)
    return $ FunctionCall (Name var) args

exp :: Parser Expression
exp = buildExpressionParser operators prefixexp
