import Text.Parsec
import Text.Parsec.String
import System.Environment
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
parseHeader' = lookAhead parseHeader >> return ()

parseFooter :: Parser ()
parseFooter = string "Generating HTML ... Done\n" >> return ()

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
            return (x' ++ "-> " ++ x3)
        Nothing -> return x'

parseValue :: Parser (Maybe (String, String))
parseValue = do
    name <- parseName
    signature <- parseSignature
    case head name of
        '\'' -> return Nothing
        _ -> return $ Just (name, signature)

parseModule :: Parser (String, [Maybe (String, String)])
parseModule = do
    name <- parseHeader
    values <- manyTill parseValue (try parseFooter <|> parseHeader')
    return (name, values)

main :: IO ()
main = do
    (f:_) <- getArgs
    inp <- readFile f
    case parse (many parseModule) "" inp of
        Left err -> print err
        Right stuff -> mapM_ (\(s, ss) -> do {putStrLn $ "\nMODULE: " ++ s ++ "\n"; mapM_ (\(n, v) -> putStrLn $ n ++ " : " ++ v) (catMaybes ss)}) stuff