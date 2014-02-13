import Control.Applicative
import Control.Monad
import Text.JSON
import System.Environment

(!) :: (JSON a) => JSObject JSValue -> String -> Result a
(!) = flip valFromObj

data Module = Module
    { name :: String
    , values  :: [Value] } deriving (Show)

data Value = Value
    { valuename :: String
    , valuetype :: Type
    } deriving (Show)

data Type = Function {functionArgs :: [Type], result :: Type}
          | Var      {varname :: String} 
          | Data     {dataname :: String, dataArgs :: [Type]}
          | Record   {fields :: [Field], extension :: Maybe String}
          deriving (Show)

data Field = Field { fieldname :: String, fieldtype :: Type} deriving (Show)          

data Docs = Docs [Module] | Doc Module deriving (Show)

instance JSON Module where
    showJSON = undefined

    readJSON (JSObject obj) =
        Module          <$>
        obj ! "name"    <*>
        obj ! "values"
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
    mapM_ print ms
    