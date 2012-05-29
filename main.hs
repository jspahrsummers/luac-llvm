module Main where

import Generator
import IO
import Parser
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec

printErrorAndExit e = do
    putStrLn (show e)
    exitFailure

main :: IO ()
main = do
    outFD <- openFile "output.ll" WriteMode
    contents <- getFileContents "input.lua"
    either printErrorAndExit (putTopLevelExpression outFD) (parse Parser.exp "" contents)
