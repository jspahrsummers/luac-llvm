module Main where

import Generator
import IO
import System.IO
import qualified Parser

main :: IO ()
main = do
    outFD <- openFile "output.ll" WriteMode
    contents <- getFileContents "input.lua"
    either (putStrLn . show) (putTopLevelExpression outFD) (Parser.compileExpression contents)
