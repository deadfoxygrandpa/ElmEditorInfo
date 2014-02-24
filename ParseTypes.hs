module ParseTypes (parseModules) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.Maybe (catMaybes)

parseName :: Parser String
parseName = do
    name <- many $ noneOf " "
    many1 space
    char ':'
    return name 

parseHeader :: Parser String
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

parseSignature :: Parser String
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

parseValue :: Parser (Maybe (String, String))
parseValue = do
    name <- parseName
    signature <- parseSignature
    case head name of
        '\'' -> return Nothing
        _ -> return $ Just (name, signature)

parseModule :: Parser (String, [(String, String)])
parseModule = do
    name <- optionMaybe parseHeader
    name' <- case name of
        Just n -> return n
        Nothing -> return ""
    values <- manyTill parseValue (try parseFooter <|> parseHeader')
    return (name', catMaybes values)

parseModules :: String -> Either ParseError [(String, [(String, String)])]
parseModules = parse (many parseModule) ""
