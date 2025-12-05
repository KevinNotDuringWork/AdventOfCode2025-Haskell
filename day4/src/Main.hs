{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import qualified Data.Vector as V   
import qualified Data.Vector.Unboxed as UV

type Grid = V.Vector (UV.Vector Int)

chr2Int :: Char -> Int
chr2Int '.' = 0
chr2Int '@' = 1
chr2Int _   = 0

getV :: Grid -> Int -> Int -> Int
getV grid y x
  | y < 0 || y >= V.length grid = 0
  | x < 0 || x >= UV.length row = 0
  | otherwise = row UV.! x
  where row = grid V.! y

getSum :: Grid -> Int -> Int -> Int
getSum grid y x = sum
  [ getV grid (y + dy) (x + dx)
  | dy <- [-1..1]
  , dx <- [-1..1]
  , (dy, dx) /= (0, 0)
  ]

erode :: (Int -> Int)
erode v = 0

threshold :: Int -> Int
threshold v = if v >= 4 then 0 else 1

applyMask :: (Int -> Int) -> Grid -> Grid -> Grid
applyMask f = V.zipWith (UV.zipWith apply)
  where
    apply 0 _ = 0          
    apply 1 v = f v

applyMaskPass :: (Int -> Int) -> Grid -> Grid -> Grid
applyMaskPass f = V.zipWith (UV.zipWith apply)
  where
    apply 0 v = v          
    apply 1 v = f v

listToGrid :: [[Int]] -> Grid
listToGrid = V.fromList . map UV.fromList

erosion :: Grid -> Int
erosion grid = 
  if result > 0 then 
    result + (erosion $ applyMaskPass erode affected grid)
  else 0
  where 
    height = V.length grid
    width = UV.length (grid V.! 0)
    values = V.generate height $ \y ->
        UV.generate width $ \x ->
          getSum grid y x

    affected = applyMask threshold grid values
    result = V.sum $ V.map UV.sum affected

main :: IO ()
main = do
  fileLines <- lines <$> readFile "input.txt"
  let grid = listToGrid $ map (map chr2Int) fileLines
  let result = erosion grid
  print result