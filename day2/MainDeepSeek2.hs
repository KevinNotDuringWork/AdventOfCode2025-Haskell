{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (foldl')

splitCommas :: T.Text -> [T.Text]
splitCommas = T.splitOn ","

-- O(n) solution using rolling window
isInvalid2 :: T.Text -> Bool
isInvalid2 pid =
    any checkPrefix [1..pidLen `div` 2]
  where
    pidLen = T.length pid
    
    checkPrefix len
        | pidLen `rem` len /= 0 = False  -- Must divide evenly
        | otherwise = isRepeating len
    
    isRepeating len = 
        let repeats = pidLen `div` len
            firstChunk = T.take len pid
        in all (\i -> T.take len (T.drop (i * len) pid) == firstChunk) [1..repeats-1]

-- Even faster: check from smallest to largest, stop early
isInvalid2' :: T.Text -> Bool
isInvalid2' pid = go 1
  where
    pidLen = T.length pid
    half = pidLen `div` 2
    
    go len
      | len > half = False
      | pidLen `rem` len /= 0 = go (len + 1)
      | isRepeating len = True
      | otherwise = go (len + 1)
    
    isRepeating len = 
        let firstChunk = T.take len pid
        in all (\i -> T.take len (T.drop (i * len) pid) == firstChunk) 
               [1..pidLen `div` len - 1]

isInvalid1 :: T.Text -> Bool
isInvalid1 pid =
    T.take half pid == T.drop half pid
  where
    half = T.length pid `div` 2

checkRangeWith :: (T.Text -> Bool) -> T.Text -> [T.Text]
checkRangeWith isValid range =
  filter isValid $ map (T.pack . show) [start .. end]
  where
    (left, rest) = T.break (== '-') range
    right = T.drop 1 rest
    start = read (T.unpack left) :: Int
    end   = read (T.unpack right) :: Int

main :: IO()
main = do
  content <- T.readFile "input.txt"
  let pidRanges = splitCommas content
  
  -- Time each part separately
  putStrLn "Starting part 1..."
  let result1 = sum $ map (read . T.unpack) $ concatMap (checkRangeWith isInvalid1) pidRanges
  
  putStrLn "Starting part 2..."
  let result2 = sum $ map (read . T.unpack) $ concatMap (checkRangeWith isInvalid2') pidRanges
  
  putStrLn "----- Result -----"
  print result1
  print result2