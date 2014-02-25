module Dependencies where

import Text.JSON
import Control.Applicative
import System.IO.Unsafe
import System.FilePath
import Data.List.Split
import Data.List

type Name = String
type Version = String

data Dependencies = Dependencies [(Name, Version)] deriving (Show)

instance JSON Dependencies where
    showJSON = undefined

    readJSON (JSObject obj) =
        Dependencies <$> pure x
        where x = case valFromObj "dependencies" obj :: Result JSValue of
                      Ok (JSObject ds) -> map transform $ fromJSObject ds
                      _                -> undefined

f :: FilePath
f = "C:\\Users\\aneslusan\\Documents\\GitHub\\Beer\\elm_dependencies.json"

json = unsafePerformIO $ readFile f

deps = case decode json :: Result Dependencies of
    Ok x -> x
    _ -> undefined

transform :: (String, JSValue) -> (String, String)
transform (name, (JSString s)) = (name, fromJSString s)

toFilePaths :: Dependencies -> [FilePath]
toFilePaths (Dependencies ds) =
    map (\d -> "elm_dependencies" </> name d </> version d) ds
    where name (n, _) = replace "/" "-" n
          version (_, v) = v

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old