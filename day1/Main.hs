{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace 
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.List (scanl, scanl')

instVal :: String -> Int
instVal ins = read (tail ins) :: Int  

dial :: Int -> Int -> Int
dial st del = mod (st + del) 100 

{-- 
  Evaluate a final LEFT cross over event
  - entering into 0 is a "click" hence 1 
  - exiting out of 0 is not counted 
--}

evalRemainder :: Int -> Int -> Int 
evalRemainder st del 
  | st > 0, st + del <= 0 = 1
  | st + del > 0  = 0 
  | otherwise = 0 

countZero :: Int -> Int -> Int 
countZero st del
  | del == 0 = 0 
  | del > 0 = div (st + del) 100 
  | otherwise = div (-del) 100 + evalRemainder st (rem del 100) 
 
dial2 :: Int -> Int -> (Int, Int)
dial2 st del =(dial st del, countZero st del)
  
intrp :: Char -> Int -> Int
intrp 'L' val = -val 
intrp 'R' val = val   

main :: IO()
main = do
  instructions <- fmap Text.lines (Text.readFile "input.txt") 
  let
    result2 = scanl' (\idx ins ->
      dial2 (fst idx) $ intrp (head ins) $ instVal ins 

      {-- in trace (
        "idx=" ++ show idx ++ "\t" ++
        "ins=" ++ show ins ++ "\t" ++   
        "res=" ++ show res
      ) --} 

      ) (50, 0) (map Text.unpack instructions)
      
    zeros = filter (\x -> fst x == 0) result2 
    cross = sum (map snd result2)

  print $ length zeros
  print cross
