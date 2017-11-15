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

main = do

	keywords	<- (sort . lines) <$> readFile "dblp/keywords.txt"

	cdblp <- map (\x -> let y = map (replace "\"" "") $ splitOn "\t" x in (y!!1,replace "/" "" $ replace " " "-" $ y!!2,y!!3) ) <$> drop 1 . lines <$> readFile "dblp/cdblp.csv"
	jdblp <- map (\x -> let y = map (replace "\"" "") $ splitOn "\t" x in (y!!1,replace "/" "" $ replace " " "-" $ y!!2,y!!4) ) <$> drop 1 . lines <$> readFile "dblp/jdblp.csv"

	let 
		cdblp_aggregated = map (\(y,v,t) -> if (aaai_symposium v) then (y,"AAAI-Symposium",t) else if (aaai_workshop v) then (y,"AAAI-Workshop",t) else (y,v,t)) cdblp where
			aaai_symposium v = (isInfixOf "AAAI-Fall-Symposium" v) || (isInfixOf "AAAI-Spring-Symposium" v)
			aaai_workshop v = isInfixOf "AAAI-Workshop" v

		jdblp_aggregated = map (\(y,v,t) -> if ieee_trans_ai v then (y,"IEEE-Trans.-AI",t) else if ieee_trans_robotics v then (y,"IEEE-Trans.-Robotics",t) else if ieee_trans v then (y,"IEEE-Trans.",t) else if acm_trans v then (y,"ACM-Trans.",t) else (y,v,t)) jdblp where
			ieee_trans_ai v = 
				isInfixOf "IEEE-Trans.-Affective" v 							||
				isInfixOf "IEEE-Trans.-Audio,-Speech" v 						||
				isInfixOf "IEEE-Trans.Cognitive" v 								||
				isInfixOf "IEEE-Trans.-Comput.-Intellig." v						||
				isInfixOf "IEEE-Trans.Emerging-Topics-in-Comput.-Intellig" v	||
				isInfixOf "IEEE-Trans.-Fuzzy-Systems" v							||
				isInfixOf "IEEE-Trans.-Intelligent"	v							||
				isInfixOf "IEEE-Trans.-Neural" v								

			ieee_trans_robotics v =
				isInfixOf "IEEE-Trans.-Automat." v								||
				isInfixOf "IEEE-Trans.-Automation"	v							||
				isInfixOf "IEEE-Trans.-Robotics" v

			ieee_trans v = isInfixOf "IEEE-Trans." v

			acm_trans v = isInfixOf "ACM-Trans." v

	let cdblp_keywords = map (\x -> (snd3 $ head x, map (\y -> (thd3 $ head y, map ((/(fromIntegral $ length y)) . fromIntegral) $ accumulate $ map fst3 y)) $ groupBy (\a b -> (thd3 a)==(thd3 b) ) $ sortBy (comparing thd3) x )) $ groupBy (\a b -> (snd3 a) == (snd3 b)) $ sortBy (comparing snd3) $ map (\x -> (find_keywords (fst3 x) keywords, snd3 x, thd3 x)) cdblp_aggregated
	let jdblp_keywords = map (\x -> (snd3 $ head x, map (\y -> (thd3 $ head y, map ((/(fromIntegral $ length y)) . fromIntegral) $ accumulate $ map fst3 y)) $ groupBy (\a b -> (thd3 a)==(thd3 b) ) $ sortBy (comparing thd3) x )) $ groupBy (\a b -> (snd3 a) == (snd3 b)) $ sortBy (comparing snd3) $ map (\x -> (find_keywords (fst3 x) keywords, snd3 x, thd3 x)) jdblp_aggregated

	mapM_ (\(venue,history) -> writeFile (printf "stats/conf/%s" venue)		( unlines $ (intercalate " " $ "year":keywords) : map (\(year,frequencies) -> let frequencies' = (sum frequencies) : frequencies in year ++ " " ++ (intercalate " " $ map show frequencies') ) history) ) cdblp_keywords
	mapM_ (\(venue,history) -> writeFile (printf "stats/journal/%s" venue)	( unlines $ (intercalate " " $ "year":keywords) : map (\(year,frequencies) -> let frequencies' = (sum frequencies) : frequencies in year ++ " " ++ (intercalate " " $ map show frequencies') ) history) ) jdblp_keywords