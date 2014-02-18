
import Control.Monad
import Text.JSON
import System.Environment
import qualified Data.Text as T

import ParseJSON

strip :: String -> String
strip = T.unpack . T.strip . T.pack

values :: Module -> [String]
values (Module name values) =
	map (\(Value n t) -> name ++ "." ++ n ++ " : " ++ show' t) values

show' :: Type -> String
show' (Function args result) = (concatMap (\arg -> strip (show' arg) ++ " -> ") args) ++ strip (show' result)
show' (Var name) = name
show' (Data name args) = case name of
	"_List" -> "[" ++ (concatMap show' args) ++ "]"
	_ -> name ++ " " ++ (concatMap show' args)
show' (Record fields extension) = "{" ++ (concatMap (\(Field n t) -> n ++ ": " ++ show' t) fields) ++ "}"

main :: IO ()
main = do
    (f:_) <- getArgs
    file <- readFile f
    stuff <- case (decode file :: Result Docs) of
        Ok idk -> return idk
        _ -> undefined
    ms <- case stuff of
        Docs x -> return x
        Doc  x -> return [x]
    mapM_ (\m -> print $ values m) ms