{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

splitCommas :: String -> [String]
splitCommas "" = []
splitCommas s =
  let (before, after) = break (== ',') s
  in before : case after of
      [] -> []
      (_:rest) -> splitCommas rest

isInvalid2 :: Int -> String -> Bool
isInvalid2 num pid =

  {-- } 
  trace ("\n\n isInvalid2: num=" ++ show num ++
  ", pid=" ++ show pid ++
  ", fragment=" ++ show fragment ++ 
  ", rest=" ++ show rest ++ 
  ", equality" ++ show (fragment == rest)) $
  --}

  (num <= limit) && ((rem rest_len num == 0 && fragment == rest)
          || isInvalid2 (num + 1) pid)

  where
    (seed, rest) = splitAt num pid
    rest_len = length rest
    fragment = take (length rest) $ cycle seed
    limit = div (length pid) 2

isInvalid1 :: String -> Bool
isInvalid1 pid =
    a == b
  where
    (a, b) = splitAt (div (length pid) 2) pid

checkRangeWith :: (String -> Bool) -> String -> [String]
checkRangeWith isValid range =
  filter isValid $ map show [start .. end]
  where
    (left, rest) = break (== '-') range
    right = drop 1 rest
    start = read left :: Int
    end   = read right :: Int

main :: IO()
main = do
  content <- readFile "input.txt"
  let pidRanges = splitCommas content
  
  let result1 = sum $ map (read::String->Int) $ concatMap (checkRangeWith isInvalid1) pidRanges
  let result2 = sum $ map (read::String->Int) $ concatMap (checkRangeWith (isInvalid2 1)) pidRanges
  
  print "----- Result -----"
  print result1
  print result2
