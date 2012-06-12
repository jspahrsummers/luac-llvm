{- Contains definitions for AST types -}
module AST where

-- Represents any Lua identifier
newtype Name = Name String

instance Show Name where
    show (Name s) = s

data BinaryOperator = AddOperator | SubOperator
    deriving Show

data Expression =
    NilLiteral |
    NotExpression Expression |
    BooleanLiteral Bool |
    NumberLiteral Double |
    BinaryExpression BinaryOperator Expression Expression |
    FunctionCall Name [Expression]
    deriving Show
