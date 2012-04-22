module Generator where

import Parser
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
    hPutStr fd (show num)
