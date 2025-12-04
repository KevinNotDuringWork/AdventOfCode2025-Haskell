{-# LANGUAGE OverloadedStrings #-}

import Data.List (nubBy, sortBy)
import Data.Function (on)
import qualified Data.Set as Set
import Debug.Trace

uniqueByFirst :: [(Char, Int)] -> [(Char, Int)]
uniqueByFirst = nubBy ((==) `on` fst)

valuesAt :: (Int, Int) -> String -> Int
valuesAt (x, y) s = read [s !! x, s !! y]

allPairs :: [Int] -> [(Int, Int)]
allPairs xs = [(x, y) | x <- xs, y <- xs, x < y]

allJolts :: [String] -> [(Int, Int)] -> [Set.Set Int]
allJolts banks comb = [Set.fromList $ map (`valuesAt` x) comb | x <- banks]

maxV :: String -> Int -> String
maxV st runway
  | runway <= 0 || st == "" = "0"
  | otherwise =

    {-- trace(
      "  st=" ++ show st ++ 
      ", stLen=" ++ show (length st) ++ 
      ", runway=" ++ show runway ++ 
      ", penalties=" ++ show penalties ++ 
      ", mr=" ++ show mr
    ) $ --}
    
    show $ maximum $ map (read :: String -> Int) mr
      where

        {-- prune results that don't make sense --}
        {-- prune penalties: why pay higher price for the same value?? --}
        {-- Crucial insight is that you must choose the highest in that window --}
        penalties = take 1 $ sortBy (flip compare `on` fst) $
          take (length st - runway + 1) $ zip st [1..length st]
        
        mr = map (\x -> fst x : maxV (drop (snd x) st) (runway - 1)) penalties

main :: IO()
main = do
  banks  <- fmap lines (readFile "input.txt")

  {-- All combinations of switches --} 
  let allComb = allPairs [0..length (head banks) - 1]   

  {-- Results of banks x combinations --}
  let result = allJolts banks allComb 

  {-- Part 1 --}
  print $ sum $ map maximum result

  {-- Part 2 --}
  print $ sum $ map ((read :: (String -> Int)) . take 12 . flip maxV 12) banks
