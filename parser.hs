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
braces = Token.braces lexer
brackets = Token.brackets lexer
identifier = Token.identifier lexer
number = Token.naturalOrFloat lexer
whiteSpace = Token.whiteSpace lexer
commaSep = Token.commaSep lexer
string = Token.stringLiteral lexer
symbol = Token.symbol lexer

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

{-
    exp ::=  nil | false | true | Number | String | ‘...’ | functiondef | 
             prefixexp | tableconstructor | exp binop exp | unop exp 

    prefixexp ::= var | functioncall | ‘(’ exp ‘)’
-}
prefixexp =
    parens Parser.exp <|>
    functioncall <|>
    tableconstructor <|>
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

name = do
    str <- identifier
    return $ Name str

-- prefixexp args | prefixexp ‘:’ Name args
functioncall = do
    var <- name
    spaces
    args <- parens (commaSep Parser.exp)
    return $ FunctionCall var args

-- ‘{’ [field {fieldsep field} [fieldsep]] ‘}’
tableconstructor = do
    fieldList <- braces (field `sepEndBy` fieldsep)
    return $ TableConstructor fieldList

field = expressionKeyField <|> nameKeyField <|> implicitKeyField

-- ‘[’ exp ‘]’ ‘=’ exp
expressionKeyField = do
    spaces
    key <- brackets Parser.exp

    spaces
    symbol "="

    value <- Parser.exp
    spaces

    return $ TableField (TableExpressionKey key) value

-- Name ‘=’ exp
nameKeyField = do
    spaces
    key <- name

    spaces
    symbol "="

    value <- Parser.exp
    spaces

    return $ TableField (TableNameKey key) value

-- exp
implicitKeyField = do
    spaces
    value <- Parser.exp
    spaces

    return $ TableField TableImplicitKey value

-- ‘,’ | ‘;’
fieldsep = symbol "," <|> symbol ";"

exp :: Parser Expression
exp = buildExpressionParser operators prefixexp
