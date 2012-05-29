{- Contains functions that generate LLVM assembly from an AST -}
module Generator where

import AST
import IO
import System.Directory
import System.IO

data GeneratorState = GeneratorState {
    counter :: Int,
    outputHandle :: Handle,
    tmpHandle :: Handle
}

putStatement :: GeneratorState -> String -> IO GeneratorState
putStatement s line = do
    hPutStrLn (tmpHandle s) ("\t" ++ line)
    return s

putLabel :: GeneratorState -> String -> IO GeneratorState
putLabel s name = do
    hPutStrLn (tmpHandle s) (name ++ ":")
    return s

putExpression :: GeneratorState -> Expression -> IO GeneratorState

putExpression s (NumberLiteral num) = do
    putStatement s ("call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show num) ++ ")")

putExpression s (NotExpression expr) = do
    s2 <- putExpression s expr

    let c1 = counter s2
        c2 = (c1 + 1)
        finalState = GeneratorState {
            counter = c2 + 1,
            tmpHandle = tmpHandle s2,
            outputHandle = outputHandle s2
        }

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
    tmpFD <- openFile "tmp.ll" ReadWriteMode

    let s = GeneratorState { counter = 1, tmpHandle = tmpFD, outputHandle = fd }
    putExpression s exp

    hFlush tmpFD
    hSeek tmpFD AbsoluteSeek 0

    header <- getFileContents "header.ll"
    hPutStrLn fd header

    body <- hGetContents tmpFD
    hPutStrLn fd body

    footer <- getFileContents "footer.ll"
    hPutStrLn fd footer

    hClose fd
    hClose tmpFD
    removeFile "tmp.ll"

