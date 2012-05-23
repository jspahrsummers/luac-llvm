{- Contains definitions for AST types -}
module AST where

-- Represents any Lua identifier
newtype Name = Name String deriving Show

data BinaryOperator = AddOperator | SubOperator
    deriving Show

data Expression =
    NotExpression Expression |
    NumberLiteral Double |
    BinaryExpression BinaryOperator Expression Expression |
    FunctionCall Name
    deriving Show
