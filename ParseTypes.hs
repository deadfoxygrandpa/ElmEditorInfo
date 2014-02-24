module ParseTypes where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Text.JSON

type Name = String
type Signature = String
data Module = Module Name [Value] deriving (Show)
data Value = Value Name Signature deriving (Show)


-- Output formatting stuff

showLines :: [Module] -> [String]
showLines = map showModule
    where showModule (Module name values) = intercalate "\n" $ map (\(Value n s) -> name ++ "." ++ n ++ " : " ++ s) values

instance JSON Module where
    showJSON (Module name values) = JSObject . toJSObject $
        [ ("module_name", JSString . toJSString $ name)
        , ("values", JSObject . toJSObject . valuesJSON $ values)
        ]

    readJSON = undefined

valuesJSON :: [Value] -> [(String, JSValue)]
valuesJSON = map valueJSON

valueJSON :: Value -> (String, JSValue)
valueJSON (Value name signature) = (name, JSString . toJSString $ signature)


-- Parsing stuff

parseName :: Parser Name
parseName = do
    name <- many $ noneOf " "
    many1 space
    char ':'
    return name 

parseHeader :: Parser Name
parseHeader = do
    char '['
    many $ noneOf "]"
    char ']'
    string " Compiling "
    name <- many $ noneOf " "
    many $ noneOf "\n"
    string "\n\n"
    return name

parseHeader' :: Parser ()
parseHeader' = void $ lookAhead parseHeader

parseFooter :: Parser ()
parseFooter = void $ string "Generating HTML ... Done\n"

parseSignature :: Parser Signature
parseSignature = do
    optional . many . oneOf $ "\n\t "
    x <- many $ noneOf ",\n->"
    comma <- optionMaybe $ char ','
    x' <- case comma of
            Just _ -> do
                optional . many . oneOf $ "\n\t "            
                x2 <- parseSignature
                return (x ++ ", " ++ x2)
            Nothing -> return x
    optional . many . oneOf $ "\n\t "
    arr <- optionMaybe $ string "->"
    case arr of
        Just _ -> do 
            x3 <- parseSignature
            pad <- case last x' of
                ' ' -> return ""
                _   -> return " "
            return (x' ++ pad ++ "-> " ++ x3)
        Nothing -> return x'

parseValue :: Parser (Maybe Value)
parseValue = do
    name <- parseName
    signature <- parseSignature
    case head name of
        '\'' -> return Nothing
        _ -> return . Just $ Value name signature

parseModule :: Parser Module
parseModule = do
    name <- optionMaybe parseHeader
    name' <- case name of
        Just n -> return n
        Nothing -> return ""
    values <- manyTill parseValue (try parseFooter <|> parseHeader')
    return $ Module name' (catMaybes values)

parseModules :: String -> Either ParseError [Module]
parseModules = parse (many parseModule) ""
