module Generator where

import AST
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
expression :: Handle -> Expression -> IO ()

expression fd (NumberLiteral num) = do
    statement fd ("call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show num) ++ ")")

expression fd (NotExpression expr) = do
    expression fd expr
    statement fd ("%value = call %lua_toboolean_fp @lua_toboolean (%lua_State* %state, i32 -1)")
    statement fd ("call %pop_fp @pop (%lua_State* %state, i32 1)")
    statement fd ("%negatedValue = xor i32 %value, 1")
    statement fd ("call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 %negatedValue)")

-- TODO: add real handling of the function name here 
expression fd (FunctionCall _) = do
    statement fd ("%dofile = getelementptr inbounds %dofile_t* @dofile, i64 0, i64 0")
    statement fd ("call %getglobal_fp @getglobal (%lua_State* %state, i8* %dofile)")
    statement fd ("call %lua_call_fp @lua_call (%lua_State* %state, i32 0, i32 0)")
