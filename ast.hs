module AST where

-- Represents a binary operator in an AST
data BinaryOperator = AddOperator | SubOperator
    deriving Show

-- Represents any Lua identifier
newtype Name = Name String deriving Show

-- Represents any expression in an AST
data Expression =
    -- Unary "not" expression
    NotExpression Expression |
    
    -- A literal number value
    NumberLiteral Double |

    -- A binary expression
    BinaryExpression BinaryOperator Expression Expression |

    -- A function call
    -- We'll want arguments to correspond to the function's arguments later
    FunctionCall Name

    deriving Show
