{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as TextRead

-- Constants
dialSize :: Int
dialSize = 100

startPosition :: Int
startPosition = 50

-- Data record for the state of the dial
data DialState = DialState {
  position :: Int,
  crossings :: Int
} deriving (Show)

-- Parses an instruction from a Text object, e.g., "L10" -> ('L', 10)
-- This is much more efficient than converting to String and using read.
parseInstruction :: Text.Text -> (Char, Int)
parseInstruction txt =
  let
    op = Text.head txt
    valStr = Text.tail txt
    Right (val, _) = TextRead.decimal valStr
  in (op, val)

-- Interprets the rotation character ('L' or 'R') and value to get a delta.
getRotationDelta :: Char -> Int -> Int
getRotationDelta 'L' val = -val
getRotationDelta 'R' val = val
getRotationDelta _ _ = 0 -- Should not happen with valid input

-- Calculates the new position of the dial after a rotation.
rotateDial :: Int -> Int -> Int
rotateDial start delta = mod (start + delta) dialSize

-- Counts the number of times the dial crosses 0 during a rotation.
-- This is for part 2 of the puzzle.
countZeroCrossings :: Int -> Int -> Int
countZeroCrossings _ 0 = 0
countZeroCrossings start delta
  | delta > 0 = (start + delta) `div` dialSize
  | otherwise = (-delta) `div` dialSize + countCrossingOnRemainder start (delta `rem` dialSize)

-- Helper for countZeroCrossings to handle the final partial rotation.
-- A crossing is counted if we start at a positive position and the rotation
-- makes the position less than or equal to 0.
countCrossingOnRemainder :: Int -> Int -> Int
countCrossingOnRemainder start remainder
  | start > 0 && start + remainder <= 0 = 1
  | otherwise = 0

-- Processes a single instruction and updates the dial state.
-- Works directly with Text for better performance.
processInstruction :: DialState -> Text.Text -> DialState
processInstruction currentState ins =
  let
    (rotationChar, instructionValue) = parseInstruction ins
    delta = getRotationDelta rotationChar instructionValue
    
    newPosition = rotateDial (position currentState) delta
    newCrossings = countZeroCrossings (position currentState) delta
    
  in DialState { position = newPosition, crossings = newCrossings }

main :: IO()
main = do
  -- Read instructions from file as Text
  instructionLines <- fmap Text.lines (Text.readFile "input.txt")

  let
    -- The initial state of the dial
    initialState = DialState { position = startPosition, crossings = 0 }

    -- Use scanl to get the state of the dial after each instruction.
    -- The result is a list of DialState, including the initial state.
    dialStates = scanl processInstruction initialState instructionLines

    -- Part 1: Count how many times the dial is at position 0 after a rotation.
    -- We ignore the initial state for this count, hence `tail`.
    finalPositions = map position (tail dialStates)
    passwordPart1 = length $ filter (== 0) finalPositions

    -- Part 2: Sum all the zero crossings that occurred during rotations.
    passwordPart2 = sum $ map crossings dialStates

  putStrLn $ "Part 1 Password: " ++ show passwordPart1
  putStrLn $ "Part 2 Password: " ++ show passwordPart2
