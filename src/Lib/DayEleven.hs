module Lib.DayEleven (solveDayEleven) where

type Stone = Int

type Line = [Stone]

splitNumber :: Int -> [Int]
splitNumber n =
  let numDigits = length (show n)
      halfShift = numDigits `div` 2
      divisor = 10 ^ halfShift
      firstHalf = n `div` divisor
      secondHalf = n `mod` divisor
  in [firstHalf, secondHalf]

countDigits :: Int -> Int
countDigits n
  | n < 10 = 1
  | otherwise = 1 + countDigits (n `div` 10)

evenDigits :: Int -> Bool
evenDigits n = even (countDigits n)

evolve :: Stone -> [Stone]
evolve s
  | s == 0 = [1]
  | evenDigits s = splitNumber s
  | otherwise = [2024*s]

blink :: Line -> Line
blink = concatMap evolve

blinks :: Int -> Line -> Line
blinks n = head . reverse . take n . drop 1 . iterate blink

solvePartOne :: Line -> Int
solvePartOne = length . blinks 25

solvePartTwo :: Line -> Int
solvePartTwo = length . blinks 75

solveDayEleven :: IO ()
solveDayEleven = do
  line <- readFileToLine "data/dayEleven.txt"
  print $ solvePartOne line
  print $ solvePartTwo line
  print "solved day eleven..."

readFileToLine :: FilePath -> IO Line
readFileToLine filePath = do
  content <- readFile filePath
  return $ map read $ words content