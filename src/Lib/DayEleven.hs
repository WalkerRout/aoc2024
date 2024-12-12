module Lib.DayEleven (solveDayEleven) where

import qualified Data.IntMap as IntMap

import Data.List (foldl')
import Data.IntMap (IntMap)

type Stone = Int

type Count = Int

type StoneMap = IntMap Count

splitNumber :: Int -> (Int, Int)
splitNumber n =
  let numDigits = countDigits n
      halfShift = numDigits `div` 2
      divisor = 10 ^ halfShift
      firstHalf = n `div` divisor
      secondHalf = n `mod` divisor
  in (firstHalf, secondHalf)

countDigits :: Int -> Int
countDigits n
  -- we have a single digit
  | n < 10 = 1
  -- we have a digit plus the rest of the digits in the number
  | otherwise = 1 + countDigits (n `div` 10)

evenDigits :: Int -> Bool
evenDigits n = even (countDigits n)

evolve :: Stone -> [(Stone, Count)]
evolve s
  -- we count occurrences (snd) for our new stones
  | s == 0 = [(1, 1)]
  | evenDigits s =
      let (a, b) = splitNumber s
      in [(a, 1), (b, 1)]
  | otherwise = [(2024 * s, 1)]

blink :: StoneMap -> StoneMap
-- this is a little weird going back to list, but it works..
blink stones = foldl' insertEvolved IntMap.empty (IntMap.toList stones)
  where
    -- we want to fold a map into a new map with an evolved population
    insertEvolved acc (stone, count) = foldl' (insertWithPast count) acc (evolve stone)
    -- counts of evolved stones are dependent on count of original stone (multiply)
    insertWithPast oldCount acc (newStone, newCount) = 
      -- we have oldCount stones, we want to evolve each of them, 
      -- newCount is a single stone instances next stone count
      IntMap.insertWith (+) newStone (oldCount * newCount) acc

-- perform n blinks
blinks :: Int -> StoneMap -> StoneMap
-- blinking 0 times does nothing
blinks 0 = id
-- blinking n times is the same as blinking once and then blinking n-1 times
blinks n = blinks (n - 1) . blink

numStonesAfter :: Int -> [Stone] -> Int
-- each stone has some number of occurrences, we want total count of occurrences,
-- so we just sum them together
numStonesAfter toBlink stones = IntMap.foldl' (+) 0 finalMap
  where
    initialMap = foldl' (flip addStone) IntMap.empty stones
    addStone stone acc = IntMap.insertWith (+) stone 1 acc
    finalMap = blinks toBlink initialMap

solvePartOne :: [Stone] -> Int
solvePartOne = numStonesAfter 25

solvePartTwo :: [Stone] -> Int
solvePartTwo = numStonesAfter 75

solveDayEleven :: IO ()
solveDayEleven = do
  line <- readFileToLine "data/dayEleven.txt"
  print $ solvePartOne line
  print $ solvePartTwo line
  print "solved day eleven..."

readFileToLine :: FilePath -> IO [Stone]
readFileToLine filePath = do
  content <- readFile filePath
  return $ map read $ words content
