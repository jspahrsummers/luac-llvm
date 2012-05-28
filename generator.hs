{- Contains functions that generate LLVM assembly from an AST -}
module Generator where

import AST
import IO
import System.IO

data GeneratorState = GeneratorState Int Handle

putStatement :: GeneratorState -> String -> IO GeneratorState
putStatement s@(GeneratorState c fd) line = do
    hPutStrLn fd ("\t" ++ line)
    return s

putLabel :: GeneratorState -> String -> IO GeneratorState
putLabel s@(GeneratorState c fd) name = do
    hPutStrLn fd (name ++ ":")
    return s

putExpression :: GeneratorState -> Expression -> IO GeneratorState

putExpression s (NumberLiteral num) = do
    putStatement s ("call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show num) ++ ")")

putExpression s@(GeneratorState c fd) (NotExpression expr) = do
    putExpression s expr

    let c2 = c + 1
        s2 = GeneratorState c2 fd
    putStatement s2 ("%value" ++ (show c) ++ " = call %lua_toboolean_fp @lua_toboolean (%lua_State* %state, i32 -1)")
    putStatement s2 ("call %pop_fp @pop (%lua_State* %state, i32 1)")

    let s3 = GeneratorState (c2 + 1) fd
    putStatement s3 ("%value" ++ (show c2) ++ " = xor i32 %value, 1")
    putStatement s3 ("call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 %negatedValue)")

putExpression s (FunctionCall _) = do
    putStatement s ("%dofile = getelementptr inbounds %dofile_t* @dofile, i64 0, i64 0")
    putStatement s ("call %getglobal_fp @getglobal (%lua_State* %state, i8* %dofile)")
    putStatement s ("call %lua_call_fp @lua_call (%lua_State* %state, i32 0, i32 0)")

-- Writes the file's header, the root of the AST, and the footer
putTopLevelExpression :: Handle -> Expression -> IO ()
putTopLevelExpression fd exp = do
    header <- getFileContents "header.ll"
    footer <- getFileContents "footer.ll"

    hPutStrLn fd header
    putExpression (GeneratorState 1 fd) exp
    hPutStrLn fd footer
