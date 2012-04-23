module Generator where

import qualified Parser
import System.IO

-- Writes an LLVM assembly statement
statement :: Handle -> String -> IO ()
statement fd line = do
    hPutStrLn fd ("\t" ++ line)

-- Writes an LLVM assembly label
label :: Handle -> String -> IO ()
label fd name = do
    hPutStrLn fd (name ++ ":")

-- Writes an LLVM number literal
expression :: Handle -> Parser.Expression -> IO ()

expression fd (Parser.NumberLiteral num) = do
    statement fd ("call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show num) ++ ")")

expression fd (Parser.NotExpression expr) = do
    expression fd expr
    statement fd ("%value = call %lua_toboolean_fp @lua_toboolean (%lua_State* %state, i32 -1)")
    statement fd ("call %lua_pop_fp @lua_pop (%lua_State* %state, i32 1)")
    statement fd ("%negatedValue = xor i32 %value, 1")
    statement fd ("call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 %negatedValue)")
