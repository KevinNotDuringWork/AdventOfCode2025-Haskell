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

isInvalid2 :: Int -> Int -> Bool
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
    pidStr = show pid
    (seed, rest) = splitAt num pidStr
    rest_len = length rest
    fragment = take (length rest) $ cycle seed
    limit = div (length pidStr) 2

isInvalid1  :: Int -> Bool
isInvalid1 pid =
    a == b
  where
    pidStr = show pid
    (a, b) = splitAt (div (length pidStr) 2) pidStr

checkRangeWith :: (Int -> Bool) -> String -> [Int]
checkRangeWith isValid range =
  filter isValid [start .. end]
  where
    (left, rest) = break (== '-') range
    right = drop 1 rest
    start = read left :: Int
    end   = read right :: Int

main :: IO()
main = do
  content <- readFile "input.txt"
  let pidRanges = splitCommas content
  let result1 = sum $ concatMap (checkRangeWith isInvalid1) pidRanges
  let result2 = sum $ concatMap (checkRangeWith (isInvalid2 1)) pidRanges

  print "----- Result -----"
  print result1
  print result2
