import System.Environment
import System.IO.Temp
import System.Process
import qualified Data.Text as T


split :: String -> String -> [String]
split sep s = map T.unpack $ T.splitOn sep' s'
    where sep' = T.pack sep
          s'   = T.pack s

strip :: String -> String
strip = T.unpack . T.strip . T.pack

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

main :: IO ()
main = do
    (f:_) <- getArgs
    types <- withTempDirectory "" "elm_editor_info" $ printTypes f
    putStrLn types
