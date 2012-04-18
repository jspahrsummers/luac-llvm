module Generator where

import Parser
import System.IO

-- Writes a header of common LLVM assembly definitions to a file
header :: Handle -> IO ()
header fd = do
    hPutStrLn fd "define i32 @main () {"

-- Writes a common footer
footer :: Handle -> IO ()
footer fd = do
    hPutStrLn fd "}"

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
    hPutStr fd (show num)
