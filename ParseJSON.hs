module ParseJSON where

import Control.Applicative
import Control.Monad
import Text.JSON
import System.Environment

(!) :: (JSON a) => JSObject JSValue -> String -> Result a
(!) = flip valFromObj

type Name = String
type Arg = Type
type FunctionResult = Type
type Extension = Maybe String

data Module = Module Name [Value] [Value] deriving (Show)
data Value = Value Name Type deriving (Show)
data Type = Function [Arg] FunctionResult
          | Var Name
          | Data Name [Arg]
          | Record [Field] Extension
          deriving (Show)
data Field = Field Name Type deriving (Show)         

data Docs = Docs [Module] | Doc Module deriving (Show)

instance JSON Module where
    showJSON = undefined

    readJSON (JSObject obj) =
        Module          <$>
        obj ! "name"    <*>
        obj ! "values"  <*>
        obj ! "aliases"
    readJSON _ = mzero

instance JSON Value where
        showJSON = undefined

        readJSON (JSObject obj) =
            Value <$>
            obj ! "name" <*>
            obj ! "type"
        readJSON _ = undefined

instance JSON Type where
        showJSON = undefined

        readJSON (JSObject obj) =
            case tag of
                "function" -> Function <$> obj ! "args" <*> obj ! "result"
                "var"      -> Var      <$> obj ! "name"
                "adt"      -> Data     <$> obj ! "name" <*> obj ! "args"
                "record"   -> Record   <$> obj ! "fields" <*> return Nothing
            where tag = case resultToEither (obj ! "tag") of
                            Right s -> s
                            Left _ -> undefined
        readJSON _ = undefined  

instance JSON Field where
        showJSON = undefined

        readJSON (JSArray arr) =
            Field <$>
            readJSON n <*>
            readJSON t
            where n = head arr
                  t = arr !! 1
        readJSON _ = undefined


instance JSON Docs where
        showJSON = undefined

        readJSON (JSArray array) =
            Docs <$> readJSONs (JSArray array)
        readJSON (JSObject obj) =
            Doc <$> readJSON (JSObject obj)
        readJSON _ = undefined
