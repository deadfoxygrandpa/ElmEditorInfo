
import Control.Monad
import Text.JSON
import System.Environment

import qualified ParseJSON as P

main :: IO ()
main = do
    (f:_) <- getArgs
    file <- readFile f
    stuff <- case (decode file :: Result P.Docs) of
        Ok idk -> return idk
        _ -> undefined
    ms <- case stuff of
        P.Docs x -> return x
        P.Doc  x -> return [x]
    mapM_ (\m -> print m) ms