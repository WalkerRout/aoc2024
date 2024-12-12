module Lib.DayEleven (solveDayEleven) where

import qualified Data.IntMap as IntMap

import Data.List (foldl')
import Data.IntMap (IntMap)

type Stone = Int

type Count = Int

type StoneMap = IntMap Count

splitNumber :: Int -> (Int, Int)
splitNumber n =
  let numDigits = length (show n)
      halfShift = numDigits `div` 2
      divisor = 10 ^ halfShift
      firstHalf = n `div` divisor
      secondHalf = n `mod` divisor
  in (firstHalf, secondHalf)

countDigits :: Int -> Int
countDigits n = length (show n)

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
    insertEvolved acc (stone, count) = foldl' (addStone count) acc (evolve stone)
    addStone oldCount m (newStone, newCount) = 
      IntMap.insertWith (+) newStone (oldCount * newCount) m

-- Perform n blinks
blinks :: Int -> StoneMap -> StoneMap
blinks 0 stones = stones
blinks n stones = blinks (n - 1) (blink stones)

solvePartOne :: [Stone] -> Int
solvePartOne input = IntMap.foldl' (+) 0 finalMap
  where
    initialMap = foldl' (\m stone -> IntMap.insertWith (+) stone 1 m) IntMap.empty input
    finalMap = blinks 25 initialMap

solvePartTwo :: [Stone] -> Int
solvePartTwo input = IntMap.foldl' (+) 0 finalMap
  where
    initialMap = foldl' (\m stone -> IntMap.insertWith (+) stone 1 m) IntMap.empty input
    finalMap = blinks 75 initialMap

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
