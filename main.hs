module Main where

import Generator
import IO
import Parser
import System.IO
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    outFD <- openFile "output.ll" WriteMode
    contents <- getFileContents "input.lua"
    either (putStrLn . show) (putTopLevelExpression outFD) (parse expression "" contents)
