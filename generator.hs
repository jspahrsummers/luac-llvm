{- Contains functions that generate LLVM assembly from an AST -}
module Generator where

import AST
import IO
import System.IO

data GeneratorState = GeneratorState {
    counter :: Int,
    handle :: Handle
}

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

putExpression s (NotExpression expr) = do
    s2 <- putExpression s expr

    let c1 = counter s2
        c2 = (c1 + 1)
        finalState = GeneratorState (c2 + 1) (handle s2)

    putStatement finalState ("%value" ++ (show c1) ++ " = call %lua_toboolean_fp @lua_toboolean (%lua_State* %state, i32 -1)")
    putStatement finalState ("call %pop_fp @pop (%lua_State* %state, i32 1)")
    putStatement finalState ("%value" ++ (show c2) ++ " = xor i32 %value" ++ (show c1) ++ ", 1")
    putStatement finalState ("call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 %value" ++ (show c2) ++ ")")
    return finalState

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
