{- Contains functions that generate LLVM assembly from an AST -}
module Generator where

import AST
import System.IO

putStatement :: Handle -> String -> IO ()
putStatement fd line = do
    hPutStrLn fd ("\t" ++ line)

putLabel :: Handle -> String -> IO ()
putLabel fd name = do
    hPutStrLn fd (name ++ ":")

putExpression :: Handle -> Expression -> IO ()

putExpression fd (NumberLiteral num) = do
    putStatement fd ("call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show num) ++ ")")

putExpression fd (NotExpression expr) = do
    putExpression fd expr
    putStatement fd ("%value = call %lua_toboolean_fp @lua_toboolean (%lua_State* %state, i32 -1)")
    putStatement fd ("call %pop_fp @pop (%lua_State* %state, i32 1)")
    putStatement fd ("%negatedValue = xor i32 %value, 1")
    putStatement fd ("call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 %negatedValue)")

putExpression fd (FunctionCall _) = do
    putStatement fd ("%dofile = getelementptr inbounds %dofile_t* @dofile, i64 0, i64 0")
    putStatement fd ("call %getglobal_fp @getglobal (%lua_State* %state, i8* %dofile)")
    putStatement fd ("call %lua_call_fp @lua_call (%lua_State* %state, i32 0, i32 0)")
