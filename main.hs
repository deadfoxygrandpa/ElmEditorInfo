
import Control.Monad
import Text.JSON
import System.Environment
import Data.List
import qualified Data.Text as T

import ParseJSON

strip :: String -> String
strip = T.unpack . T.strip . T.pack

values :: Module -> [String]
values (Module name values aliases) =
	(map (\(Value n t) -> name ++ "." ++ n ++ " : " ++ pick t) values) ++
	(map (\(Value n t) -> name ++ "." ++ n ++ " = " ++ pick t) aliases)
	where pick t = case t of
		Function _ _ 	-> deparenthesize . stripshow $ t
		_ 				-> stripshow t

stripshow :: Type -> String
stripshow = strip . show'

deparenthesize :: String -> String
deparenthesize (x:xs) = if (x == '(') then init xs else x:xs

show' :: Type -> String
show' (Function args result) = "(" ++ (concatMap (\arg -> strip (show' arg) ++ " -> ") args) ++ strip (show' result) ++ ")"
show' (Var name) = strip name
show' (Data name args) = case take 6 name of
	"_List" 	-> "[" ++ (concatMap stripshow args) ++ "]"
	"_Tuple" 	-> "(" ++ (concat . intersperse ", " . map stripshow $ args) ++ ")"
	_       	-> name ++ " " ++ (concatMap stripshow args)
show' (Record fields extension) = "{" ++ (concat . intersperse ", " . map (\(Field n t) -> n ++ ": " ++ stripshow t) $ fields) ++ "}"

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
    mapM_ (\m -> mapM_ putStrLn . values $ m) ms