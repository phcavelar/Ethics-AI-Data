
import Data.Char
import System.Directory
import Data.String.Utils
import Control.Exception

getFullText path = readFile path
getAbstract text = filter (\x -> not $ elem x ['\"','(',')','.',',',';']) $ unlines $ drop 1 $ takeWhile ((/=) "") $ dropWhile ((/=) "ABSTRACT") $ lines text
getBody text = filter (\x -> not $ elem x ['\"','(',')','.',',',';']) $ unlines $ dropWhile ((/=) "") $ dropWhile ((/=) "ABSTRACT") $ lines text

getProb :: String -> IO Float
getProb path = do
 fulltext <- getFullText path
 let abstract = words $ getAbstract fulltext
 let body = words $ getBody fulltext
 let prob = sum $ map (\x -> (fromIntegral $ length $ filter (\y -> x==y) $ abstract) / (fromIntegral $ length abstract) ) body
 
 if (>0) $ length abstract then
  return $! prob
 else
  return $ -1.0

main = do
 
 aaai_paths <- (map (\x -> "txt/AAAItxt/" ++ x) . drop 2 . filter (not . endswith ".err")) <$> getDirectoryContents "txt/AAAItxt/"

 probs <- filter (>=0) <$> mapM getProb aaai_paths
 print $ (sum probs) / (fromIntegral $ length probs)