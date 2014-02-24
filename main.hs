
module Main where

import System.Environment

import GetDocs
import GetTypes

main :: IO ()
main = do
    (mode:outputFormat:f:_) <- getArgs
    case mode of
        "docs"  -> getDocs outputFormat f
        "types" -> getTypes outputFormat f
        _       -> putStrLn "Expected either docs or types + a filename"
