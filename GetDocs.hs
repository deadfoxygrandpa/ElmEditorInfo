module GetDocs (getDocs) where

import Text.JSON

import ParseJSON

getDocs :: String -> FilePath -> IO ()
getDocs outputFormat f = do
    file <- readFile f
    stuff <- case (decode file :: Result Docs) of
        Ok idk -> return idk
        _ -> undefined
    ms <- case stuff of
        Docs x -> return x
        Doc  x -> return [x]
    case outputFormat of
    	"lines" -> mapM_ (\m -> mapM_ putStrLn . values $ m) ms
    	"json"  -> putStrLn . encode . showJSON $ stuff
