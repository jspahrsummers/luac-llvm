{- Contains definitions for AST types -}
module AST where

-- Represents any Lua identifier
newtype Name = Name String

instance Show Name where
    show (Name s) = s

data BinaryOperator = AddOperator | SubOperator
    deriving Show

data TableKey =
    TableExpressionKey Expression |
    TableNameKey Name |
    TableImplicitKey
    deriving Show

data TableField = TableField TableKey Expression
    deriving Show

data Expression =
    NilLiteral |
    NotExpression Expression |
    BooleanLiteral Bool |
    NumberLiteral Double |
    StringLiteral String |
    BinaryExpression BinaryOperator Expression Expression |
    FunctionCall Name [Expression] |
    TableConstructor [TableField]
    deriving Show
