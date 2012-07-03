{- Contains functions that generate LLVM assembly from an AST -}
module Generator where

import AST
import IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Directory
import System.IO

data GeneratorState = GeneratorState {
    counter :: Int,
    headerHandle :: Handle,
    bodyHandle :: Handle
}

data TableConstructorState = TableConstructorState {
    index :: Int
}

type GeneratorStateT = StateT GeneratorState IO
type TableConstructorStateT = StateT TableConstructorState GeneratorStateT

generatorPutStrLn :: (GeneratorState -> Handle) -> String -> GeneratorStateT ()
generatorPutStrLn f str = do
    handle <- gets f
    liftIO $ hPutStrLn handle str

headerPutStrLn :: String -> GeneratorStateT ()
headerPutStrLn = generatorPutStrLn headerHandle

bodyPutStrLn :: String -> GeneratorStateT ()
bodyPutStrLn = generatorPutStrLn bodyHandle

nextCounter :: GeneratorStateT Int
nextCounter = do
    c <- gets counter
    headerHandle <- gets headerHandle
    bodyHandle <- gets bodyHandle
    
    put $ GeneratorState {
        counter = succ c,
        headerHandle = headerHandle,
        bodyHandle = bodyHandle
    }

    return c

nextIndex :: TableConstructorStateT Int
nextIndex = do
    i <- gets index

    put $ TableConstructorState {
        index = succ i
    }

    return i

putStatement :: String -> GeneratorStateT ()
putStatement line = bodyPutStrLn $ "\t" ++ line

putLabel :: String -> GeneratorStateT ()
putLabel name = bodyPutStrLn $ name ++ ":"

putArguments :: [Expression] -> GeneratorStateT ()
putArguments [] = return ()
putArguments (expr : args) = do
    putExpression expr
    putArguments args

-- Emits code to push a string (with explicit length) onto the stack
putStringConstant :: String -> GeneratorStateT ()
putStringConstant str = do
    let strLen = length str
        strType = "[" ++ (show strLen) ++ " x i8]"

    c <- nextCounter

    headerPutStrLn $ "@string" ++ (show c) ++ " = private unnamed_addr constant " ++ strType ++ " c\"" ++ str ++ "\""
    putStatement $ "%string" ++ (show c) ++ " = getelementptr inbounds " ++ strType ++ "* @string" ++ (show c) ++ ", i64 0, i64 0"
    putStatement $ "call %lua_pushlstring_fp @lua_pushlstring (%lua_State* %state, i8* %string" ++ (show c) ++ ", i64 " ++ (show strLen) ++ ")"

putExpression :: Expression -> GeneratorStateT ()
putExpression NilLiteral = putStatement $ "call %lua_pushnil_fp @lua_pushnil (%lua_State* %state)"
putExpression (BooleanLiteral True) = putStatement $ "call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 1)"
putExpression (BooleanLiteral False) = putStatement $ "call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 0)"
putExpression (NumberLiteral num) = putStatement $ "call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show num) ++ ")"
putExpression (StringLiteral str) = putStringConstant str

putExpression (NotExpression expr) = do
    putExpression expr

    c1 <- nextCounter
    c2 <- nextCounter

    putStatement $ "%value" ++ (show c1) ++ " = call %lua_toboolean_fp @lua_toboolean (%lua_State* %state, i32 -1)"
    putStatement $ "call %pop_fp @pop (%lua_State* %state, i32 1)"
    putStatement $ "%value" ++ (show c2) ++ " = xor i32 %value" ++ (show c1) ++ ", 1"
    putStatement $ "call %lua_pushboolean_fp @lua_pushboolean (%lua_State* %state, i32 %value" ++ (show c2) ++ ")"

putExpression (FunctionCall name args) = do
    let strLen = length (show name) + 1
        strType = "[" ++ (show strLen) ++ " x i8]"

    c <- nextCounter

    headerPutStrLn $ "@string" ++ (show c) ++ " = private unnamed_addr constant " ++ strType ++ " c\"" ++ (show name) ++ "\\00\""

    putStatement $ "%string" ++ (show c) ++ " = getelementptr inbounds " ++ strType ++ "* @string" ++ (show c) ++ ", i64 0, i64 0"
    putStatement $ "call %getglobal_fp @getglobal (%lua_State* %state, i8* %string" ++ (show c) ++ ")"
    putArguments args

    putStatement $ "call %lua_call_fp @lua_call (%lua_State* %state, i32 " ++ (show $ length args) ++ ", i32 1)"

putExpression (TableConstructor fields) = do
    putStatement $ "call %lua_createtable_fp @lua_createtable (%lua_State* %state, i32 0, i32 0)"

    let tableState = TableConstructorState { index = 1 }
    evalStateT (putFields fields) tableState

putFields :: [TableField] -> TableConstructorStateT ()
putFields [] = return ()
putFields ((TableField key value) : remaining) = do
    putFieldKey key
    lift $ putExpression value

    putFields remaining

putFieldKey :: TableKey -> TableConstructorStateT ()
putFieldKey (TableExpressionKey expr) = lift $ putExpression expr
putFieldKey (TableNameKey name) = lift $ putStringConstant (show name)

putFieldKey TableImplicitKey = do
    i <- nextIndex
    lift $ putStatement ("call %lua_pushnumber_fp @lua_pushnumber (%lua_State* %state, %lua_Number " ++ (show i) ++ ".0)")

-- Writes the file's header, the root of the AST, and the footer
putTopLevelExpression :: Handle -> Expression -> IO ()
putTopLevelExpression fd exp = do
    tmpDir <- getTemporaryDirectory
    (tmpPath, tmpFD) <- openTempFile tmpDir "tmp.ll"

    let s = GeneratorState { counter = 1, bodyHandle = tmpFD, headerHandle = fd }
    runStateT (putExpression exp) s

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
