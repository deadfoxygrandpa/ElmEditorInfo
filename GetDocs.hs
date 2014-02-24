module GetDocs (getDocs) where

import Text.JSON
import Data.List
import qualified Data.Text as T

import ParseJSON

strip :: String -> String
strip = T.unpack . T.strip . T.pack

values :: Module -> [String]
values (Module name values' aliases datas) =
    format "" " : " name values' ++
    format "type " " = " name aliases ++
    concatMap (\(DataType n ts vs) -> ("data " ++ name ++ "." ++ n ++ " " ++ unwords ts) : 
        format "" " = " name vs) datas
    where 
        pick t = case t of
            Function _ _    -> deparenthesize . stripshow $ t
            _               -> stripshow t
        format prefix separator name' = 
            map (\(Value n t) -> prefix ++ name' ++ "." ++ n ++ separator ++ pick t)

stripshow :: Type -> String
stripshow = strip . show'

deparenthesize :: String -> String
deparenthesize (x:xs) = if x == '(' then init xs else x:xs

show' :: Type -> String
show' (Function args result) = "(" ++ concatMap (\arg -> strip (show' arg) ++ " -> ") args ++ strip (show' result) ++ ")"
show' (Var name) = strip name
show' (Data name args) = case take 6 name of
    "_List"     -> "[" ++ concatMap stripshow args ++ "]"
    "_Tuple"    -> "(" ++ (intercalate ", " . map stripshow $ args) ++ ")"
    _           -> name ++ " " ++ concatMap stripshow args
show' (Record fields _) = "{" ++ (intercalate ", " . map (\(Field n t) -> n ++ ": " ++ stripshow t) $ fields) ++ "}"

getDocs :: FilePath -> IO ()
getDocs f = do
    file <- readFile f
    stuff <- case (decode file :: Result Docs) of
        Ok idk -> return idk
        _ -> undefined
    ms <- case stuff of
        Docs x -> return x
        Doc  x -> return [x]
    mapM_ (\m -> mapM_ putStrLn . values $ m) ms
