module Main where

import System.IO
import qualified Generator
import qualified Parser

-- Writes out the top level construct of a file, including header and footer
writeTopLevelExpression :: Handle -> Parser.Expression -> IO ()
writeTopLevelExpression fd exp = do
    Generator.header fd
    Generator.expression fd exp
    Generator.footer fd

main :: IO ()
main = do
    inFD <- openFile "input.lua" ReadMode
    outFD <- openFile "output.ll" WriteMode
    contents <- hGetContents inFD
    either (putStrLn . show) (writeTopLevelExpression outFD) (Parser.compileExpression contents)
