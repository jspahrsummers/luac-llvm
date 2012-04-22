module Main where

import System.IO
import qualified Generator
import qualified Parser

-- Returns the contents of the text file at the given path
getFileContents :: FilePath -> IO String
getFileContents path = do
    fd <- openFile path ReadMode
    str <- hGetContents fd
    return str

-- Writes out the top level construct of a file, including header and footer
writeTopLevelExpression :: Handle -> Parser.Expression -> IO ()
writeTopLevelExpression fd exp = do
    header <- getFileContents "header.ll"
    footer <- getFileContents "footer.ll"

    hPutStrLn fd header
    Generator.expression fd exp
    hPutStrLn fd footer

main :: IO ()
main = do
    outFD <- openFile "output.ll" WriteMode
    contents <- getFileContents "input.lua"
    either (putStrLn . show) (writeTopLevelExpression outFD) (Parser.compileExpression contents)
