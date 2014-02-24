module GetTypes where

import System.IO.Temp (withTempDirectory)
import System.Process (readProcess)

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

getTypes :: FilePath ->  IO ()
getTypes f = do
    types <- withTempDirectory "" "elm_editor_info" $ printTypes f
    case parseModules types of
        Left err -> print err
        Right stuff -> mapM_ (\(s, ss) -> do {putStrLn $ "\nMODULE: " ++ s ++ "\n"; mapM_ (\(n, v) -> putStrLn $ n ++ " : " ++ v) ss}) stuff        
