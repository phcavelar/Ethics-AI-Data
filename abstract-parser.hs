{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List.Split
import Data.Tuple.Utils
import Data.List
import Data.String.Utils
import Data.Char
import Text.Printf
import System.FilePath
import Data.Ord

find_keywords :: String -> [String] -> [Int]
find_keywords text keywords = map (\x -> length $ findIndices ((==x)) $ map (map toLower) $ splitWs text) keywords

accumulate :: Num a => [[a]] -> [a]
accumulate = foldr (zipWith (+)) (repeat 0)

findYear x = head $ (filter (all isDigit) x) ++ ["0"]
findAbstract = maximumBy (comparing length)

main = do

	keywords 					<- (sort . lines) <$> readFile "dblp/keywords.txt"
	entries 					<- map (\x -> (replace ";" "" $ replace "\n" "" $ x!!0, findAbstract x)) <$> (chunksOf 10 . init . splitOn "#") <$> readFile "abstracts/aaai.txt"

	let entries_keywords 			= map (\(year,abstract) -> (year, find_keywords abstract keywords)) entries
	let entries_keywords_byyear 	= map (\x -> (fst $ head x, accumulate $ map snd x, (map (/(fromIntegral $ length x)) $ map fromIntegral $ accumulate $ map snd x)::[Double] ) ) $ groupBy (\a b -> (fst a) == (fst b)) $ sortBy (comparing fst) entries_keywords 

	writeFile "stats/abstracts-aaai.dat" $ unlines $ map (\x -> printf "%s %d %0.3f" (fst3 x) (sum $ snd3 x) (sum $ thd3 x) ) entries_keywords_byyear