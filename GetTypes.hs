module GetTypes where

import System.IO.Temp (withTempDirectory)
import System.Process (readProcess)
import Text.JSON

import ParseTypes

printTypes :: FilePath -> FilePath -> IO String
printTypes file directory = 
    readProcess "elm" 
                [ "--make"
                , "--print-types"
                , "--cache-dir=" ++ directory
                , "--build-dir=" ++ directory
                , file
                ]
                []

getTypes :: String -> FilePath -> IO ()
getTypes outputFormat f = do
    types <- withTempDirectory "" "elm_editor_info" $ printTypes f
    case parseModules types of
        Left err -> print err
        Right stuff -> case outputFormat of
            "lines" -> mapM_ putStrLn (showLines stuff)
            "json"  -> putStrLn . encode . showJSON $ stuff
