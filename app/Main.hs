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
    let width = Maybe.fromMaybe 30 . parseArgs $ args
    mappingLines <- readLines "mapping"
    inputLines <- fmap lines $ getContents
    let asciiMap = buildMap mappingLines
    -- M.forM_ inputLines (\x -> printPair (pad asciiMap width (replaceString asciiMap x)))
    M.forM_ inputLines (printPair . pad asciiMap width . replaceString asciiMap)


parseArgs :: [String] -> Maybe Int
parseArgs = firstJust isWidth

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = Maybe.listToMaybe . Maybe.mapMaybe f

isWidth :: String -> Maybe Int
isWidth ('-':'-':'w':'i':'d':'t':'h':'=':w) = TR.readMaybe w
isWidth _ = Nothing

printPair :: (String, String) -> IO ()
printPair (a, b) = putStr a >> putStrLn b

readLines :: String -> IO [String]
readLines = fmap lines . SIO.readFile

buildMap :: [String] -> Map.Map Char Char
buildMap = Map.fromList . map getMapping

getMapping :: String -> (Char, Char)
getMapping (x:y:[]) = (x, y)
getMapping _ = error "invalid input"

replaceString :: Map.Map Char Char -> String -> String
replaceString m s = foldr (\c a -> (Map.findWithDefault 'x' c m):a) "" s

makePadding :: Int -> String
makePadding n = take n . repeat $ ' '

pad :: Map.Map Char Char -> Int -> String -> (String, String)
pad m n s = (s, replaceString m p)
    where
        l = length s
        p = makePadding (n - l)
