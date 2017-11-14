{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List.Split
import Data.Tuple.Utils
import Data.List
import Data.String.Utils
import Data.Char
import Text.Printf
import System.FilePath

find_keywords :: String -> [String] -> [Int]
find_keywords text keywords = map (\x -> length $ findIndices ((==x)) $ map (map toLower) $ splitWs text) keywords

accumulate :: Num a => [[a]] -> [a]
accumulate = foldr (zipWith (+)) (repeat 0)

main = do

	keywords 					<- lines <$> readFile "dblp/keywords.txt"
	entries  					<- map (\x -> let y = splitOn "#" x in (replace ";" "" $ y!!0,y!!3,y!!9)) <$> lines <$> readFile "nips.txt"
	let entries_keywords		= filter (\x -> any (>0) $ (++) (snd3 x) (thd3 x) ) $ map (\x -> (fst3 x, find_keywords (snd3 x) keywords, find_keywords (thd3 x) keywords )) entries
	let entries_keywords_year 	= map (\x -> (fst3 $ head x, accumulate $ map (\y -> zipWith (+) (snd3 y) (thd3 y)) x)) $ groupBy (\a b -> (fst3 a) == (fst3 b)) $ sortBy (\a b -> compare (fst3 a) (fst3 b)) $ entries_keywords
	
	writeFile "stats/nips.dat" $ unlines $ map (\x -> printf "%s %s" (fst x) (intercalate " " $ map show $ snd x)) entries_keywords_year