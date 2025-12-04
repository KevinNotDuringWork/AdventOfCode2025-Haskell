{-

LOL DeepSeek recommended fixes crashed my computer. Don't know what's going on here. 


-}

{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List (foldl')

splitCommas :: Text.Text -> [Text.Text]
splitCommas = Text.splitOn ","

isInvalid2 :: Int -> Text.Text -> Bool
isInvalid2 num pid =

  {-- } 
  trace ("\n\n isInvalid2: num=" ++ show num ++
  ", pid=" ++ show pid ++
  ", fragment=" ++ show fragment ++ 
  ", rest=" ++ show rest ++ 
  ", equality" ++ show (fragment == rest)) $
  --}

  (num <= limit) && ((rem (Text.length rest) num == 0 && fragment == rest)
          || isInvalid2 (num + 1) pid)

  where
    (seed, rest) = Text.splitAt num pid
    fragment = Text.take (Text.length rest) (Text.concat (repeat seed))
    limit = div (Text.length pid) 2

isInvalid1 :: Text.Text -> Bool
isInvalid1 pid =
    a == b
  where
    (a, b) = Text.splitAt (div (Text.length pid) 2) pid

checkRangeWith :: (Text.Text -> Bool) -> Text.Text -> [Text.Text]
checkRangeWith isValid range =
  filter isValid $ map (Text.pack . show) [start .. end]
  where
    (left, rest) = Text.break (== '-') range
    right = Text.drop 1 rest
    start = read (Text.unpack left) :: Int
    end   = read (Text.unpack right) :: Int

main :: IO()
main = do
  content <- Text.readFile "input.txt"
  let pidRanges = splitCommas content
  
  let result1 = foldl' (+) 0 $ map (read . Text.unpack) $ concatMap (checkRangeWith isInvalid1) pidRanges
  let result2 = foldl' (+) 0 $ map (read . Text.unpack) $ concatMap (checkRangeWith (isInvalid2 1)) pidRanges
  
  print "----- Result -----"
  print result1
  print result2
