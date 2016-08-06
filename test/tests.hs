module Main where

import System.Exit (exitFailure)
import Parsec

main:: IO()
main = do


    putStrLn "This test always fails!"
    exitFailure

simple:: Parser Char
simple = letter
