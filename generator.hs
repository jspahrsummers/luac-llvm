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

data TableConstructorState = TableConstructorState {
    generatorState :: GeneratorState,
    nextIndex :: Int
}

putStatement :: GeneratorState -> String -> IO GeneratorState
putStatement s line = do
    hPutStrLn (tmpHandle s) ("\t" ++ line)
    return s

putLabel :: GeneratorState -> String -> IO GeneratorState
putLabel s name = do
    hPutStrLn (tmpHandle s) (name ++ ":")
    return s

putArguments :: GeneratorState -> [Expression] -> IO GeneratorState
putArguments s [] = return s
putArguments s (expr : args) = do
    nextState <- putExpression s expr
    finalState <- putArguments nextState args
    return finalState

-- Emits code to push a string (with explicit length) onto the stack
putString :: GeneratorState -> String -> IO GeneratorState
putString s str = do
    let outFD = outputHandle s
        c = counter s
        finalState = GeneratorState {
            counter = c + 1,
            tmpHandle = tmpHandle s,
            outputHandle = outFD
        }

    let strLen = length str
        strType = "[" ++ (show strLen) ++ " x i8]"

    hPutStrLn outFD ("@string" ++ (show c) ++ " = private unnamed_addr constant " ++ strType ++ " c\"" ++ str ++ "\"")

    putStatement finalState ("%string" ++ (show c) ++ " = getelementptr inbounds " ++ strType ++ "* @string" ++ (show c) ++ ", i64 0, i64 0")
    putStatement finalState ("call %lua_pushlstring_fp @lua_pushlstring (%lua_State* %state, i8* %string" ++ (show c) ++ ", i64 " ++ (show strLen) ++ ")")
    return finalState

putExpression :: GeneratorState -> Expression -> IO GeneratorState

putExpression s NilLiteral = do
    putStatement s ("call %lua_pushnil_fp @lua_pushnil (%lua_State* %state)")

putExpression s (BooleanLiteral True) = do
    putStatement s ("call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 1)")

putExpression s (BooleanLiteral False) = do
    putStatement s ("call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 0)")

putExpression s (NumberLiteral num) = do
    putStatement s ("call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show num) ++ ")")

putExpression s (StringLiteral str) = putString s str

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

putExpression s (FunctionCall name args) = do
    let outFD = outputHandle s
        c = counter s
        nextState = GeneratorState {
            counter = c + 1,
            tmpHandle = tmpHandle s,
            outputHandle = outFD
        }

    let strLen = length (show name) + 1
        strType = "[" ++ (show strLen) ++ " x i8]"

    hPutStrLn outFD ("@string" ++ (show c) ++ " = private unnamed_addr constant " ++ strType ++ " c\"" ++ (show name) ++ "\\00\"")

    putStatement nextState ("%string" ++ (show c) ++ " = getelementptr inbounds " ++ strType ++ "* @string" ++ (show c) ++ ", i64 0, i64 0")
    putStatement nextState ("call %getglobal_fp @getglobal (%lua_State* %state, i8* %string" ++ (show c) ++ ")")
    finalState <- putArguments nextState args

    putStatement finalState ("call %lua_call_fp @lua_call (%lua_State* %state, i32 " ++ (show $ length args) ++ ", i32 1)")
    return finalState

putExpression s (TableConstructor fields) = do
    putStatement s ("call %lua_createtable_fp @lua_createtable (%lua_State* %state, i32 0, i32 0)")

    let tableState = TableConstructorState { generatorState = s, nextIndex = 1 }
    finalTableState <- putFields tableState fields

    return $ generatorState finalTableState

putFields :: TableConstructorState -> [TableField] -> IO TableConstructorState
putFields s [] = return s
putFields s ((TableField key value) : remaining) = do
    s2 <- putFieldKey s key
    gs3 <- putExpression (generatorState s2) value

    let nextState = TableConstructorState {
        generatorState = gs3,
        nextIndex = (nextIndex s2)
    }

    finalState <- putFields nextState remaining
    return finalState

putFieldKey :: TableConstructorState -> TableKey -> IO TableConstructorState

putFieldKey s (TableExpressionKey expr) = do
    nextGeneratorState <- putExpression (generatorState s) expr
    return $ TableConstructorState {
        generatorState = nextGeneratorState,
        nextIndex = (nextIndex s)
    }

putFieldKey s (TableNameKey name) = do
    nextGeneratorState <- putString (generatorState s) (show name)
    return $ TableConstructorState {
        generatorState = nextGeneratorState,
        nextIndex = (nextIndex s)
    }

putFieldKey s TableImplicitKey = do
    let index = (nextIndex s)
        gs = (generatorState s)

    nextGeneratorState <- putStatement gs ("call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show index) ++ ")")
    
    return $ TableConstructorState {
        generatorState = nextGeneratorState,
        nextIndex = index + 1
    }

-- Writes the file's header, the root of the AST, and the footer
putTopLevelExpression :: Handle -> Expression -> IO ()
putTopLevelExpression fd exp = do
    tmpDir <- getTemporaryDirectory
    (tmpPath, tmpFD) <- openTempFile tmpDir "tmp.ll"

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
    removeFile tmpPath
