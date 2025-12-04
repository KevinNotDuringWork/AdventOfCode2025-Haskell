{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortBy)
import Data.Function (on)
import Debug.Trace

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

        {-- Crucial insight is that you must choose the highest in that window --}
        penalties = take 1 $ sortBy (flip compare `on` fst) $
          take (length st - runway + 1) $ zip st [1..length st]
        
        mr = map (\x -> fst x : maxV (drop (snd x) st) (runway - 1)) penalties

main :: IO()
main = do
  banks  <- fmap lines (readFile "example.txt")

  {-- Part 1 --}
  print $ sum $ map ((read :: (String -> Int)) . take 2 . flip maxV 2) banks

  {-- Part 2 --}
  print $ sum $ map ((read :: (String -> Int)) . take 12 . flip maxV 12) banks
