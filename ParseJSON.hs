module ParseJSON where

import Control.Applicative
import Control.Monad
import Text.JSON
import Data.List
import qualified Data.Text as T

(!) :: (JSON a) => JSObject JSValue -> String -> Result a
(!) = flip valFromObj

type Name = String
type Arg = Type
type FunctionResult = Type
type Extension = Maybe String
type TypeVariable = String

data Module = Module Name [Value] [Value] [DataType] deriving (Show)
data Value = Value Name Type deriving (Show)
data DataType = DataType Name [TypeVariable] [Value] deriving (Show)
data Type = Function [Arg] FunctionResult
          | Var Name
          | Data Name [Arg]
          | Record [Field] Extension
          deriving (Show)
data Field = Field Name Type deriving (Show)         

data Docs = Docs [Module] | Doc Module deriving (Show)

instance JSON Module where
    showJSON m@(Module name _ _ _) = 
        JSObject . toJSObject $
            [ ("module_name", JSString . toJSString $ name)
            , ("values", JSObject . toJSObject . values2 $ m)
            ]

    readJSON (JSObject obj) =
        Module          <$>
        obj ! "name"    <*>
        obj ! "values"  <*>
        obj ! "aliases" <*>
        obj ! "datatypes"
    readJSON _ = mzero

instance JSON Value where
    showJSON = undefined

    readJSON (JSObject obj) =
        Value           <$>
        obj ! "name"    <*>
        obj ! "type"
    readJSON _ = undefined

instance JSON DataType where
    showJSON = undefined

    readJSON (JSObject obj) =
        DataType                <$>
        obj ! "name"            <*>
        obj ! "typeVariables"   <*>
        obj ! "constructors"
    readJSON _ = undefined   

instance JSON Type where
    showJSON = undefined

    readJSON (JSObject obj) =
        case tag of
            "function" -> Function <$> obj ! "args"     <*> obj ! "result"
            "var"      -> Var      <$> obj ! "name"
            "adt"      -> Data     <$> obj ! "name"     <*> obj ! "args"
            "record"   -> Record   <$> obj ! "fields"   <*> return Nothing
        where tag = case resultToEither (obj ! "tag") of
                        Right s -> s
                        Left _ -> undefined
    readJSON _ = undefined  

instance JSON Field where
    showJSON = undefined

    readJSON (JSArray arr) =
        Field       <$>
        readJSON n  <*>
        readJSON t
        where n = head arr
              t = arr !! 1
    readJSON _ = undefined


instance JSON Docs where
    showJSON (Docs ms) = showJSONs ms
    showJSON (Doc  m ) = showJSON m

    readJSON (JSArray array) =
        Docs <$> readJSONs (JSArray array)
    readJSON (JSObject obj) =
        Doc <$> readJSON (JSObject obj)
    readJSON _ = undefined

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

values2 :: Module -> [(String, JSValue)]
values2 (Module _ values' aliases datas) =
    format values' ++
    format aliases ++
    concatMap (\(DataType n ts vs) -> (n, JSString . toJSString . unwords $ ts) : 
        format vs) datas
    where 
        pick t = case t of
            Function _ _    -> deparenthesize . stripshow $ t
            _               -> stripshow t
        format = 
            map (\(Value n t) -> (n, JSString . toJSString . pick $ t))

deparenthesize :: String -> String
deparenthesize (x:xs) = if x == '(' then init xs else x:xs

stripshow :: Type -> String
stripshow = strip . showType

strip :: String -> String
strip = T.unpack . T.strip . T.pack

showType :: Type -> String
showType (Function args result) = "(" ++ concatMap (\arg -> strip (showType arg) ++ " -> ") args ++ strip (showType result) ++ ")"
showType (Var name) = strip name
showType (Data name args) = case take 6 name of
    "_List"     -> "[" ++ concatMap stripshow args ++ "]"
    "_Tuple"    -> "(" ++ (intercalate ", " . map stripshow $ args) ++ ")"
    _           -> name ++ " " ++ concatMap stripshow args
showType (Record fields _) = "{" ++ (intercalate ", " . map (\(Field n t) -> n ++ ": " ++ stripshow t) $ fields) ++ "}"