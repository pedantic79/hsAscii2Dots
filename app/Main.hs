module Main (main) where

import qualified Control.Monad as M
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.IO as SIO
import qualified System.Environment as SE
import qualified Text.Read as TR

main :: IO ()
main = do
    args <- SE.getArgs
    mappingLines <- fmap lines . SIO.readFile $ "mapping"
    inputLines <- fmap lines $ getContents
    let width = Maybe.fromMaybe 30 . parseArgs $ args
    let asciiMap = buildMap mappingLines
    -- M.forM_ inputLines (\x -> printPair (pad asciiMap width (replaceString asciiMap x)))
    M.forM_ inputLines (printPair . pad asciiMap width . replaceString asciiMap)

parseArgs :: [String] -> Maybe Int
parseArgs = Maybe.listToMaybe . Maybe.mapMaybe isWidth

isWidth :: String -> Maybe Int
isWidth ('-':'-':'w':'i':'d':'t':'h':'=':w) = TR.readMaybe w
isWidth _ = Nothing

printPair :: (String, String) -> IO ()
printPair (a, b) = putStr a >> putStrLn b

buildMap :: [String] -> Map.Map Char Char
buildMap = Map.fromList . map getMapping

getMapping :: String -> (Char, Char)
getMapping (x:y:[]) = (x, y)
getMapping _ = error "invalid input"

replaceString :: Map.Map Char Char -> String -> String
replaceString m = foldr (\c a -> (Map.findWithDefault 'x' c m):a) ""

pad :: Map.Map Char Char -> Int -> String -> (String, String)
pad m n s = (s, replaceString m p)
    where
        p = take (n - length s) . repeat $ ' '
