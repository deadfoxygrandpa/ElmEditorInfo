
module Main where

import System.Environment

import GetDocs
import GetTypes

main :: IO ()
main = do
    (mode:f:_) <- getArgs
    case mode of
        "docs"  -> getDocs f
        "types" -> getTypes f
        _       -> putStrLn "Expected either docs or types + a filename"
