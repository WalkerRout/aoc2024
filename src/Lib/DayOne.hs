{-# LANGUAGE BangPatterns #-}

module Lib.DayOne (solveDayOne) where

import Data.List
import qualified Data.Map as Map

import System.IO

solvePartOne :: (Ord a, Num a) => [a] -> [a] -> a
solvePartOne xs ys = distanceBetween $ pairAndSort xs ys
  where
    pairAndSort :: Ord a => [a] -> [a] -> ([a], [a])
    pairAndSort xs ys = (sort xs, sort ys)

    distanceBetween :: Num a => ([a], [a]) -> a
    distanceBetween (xs, ys) = distanceBetweenHelper xs ys (fromIntegral 0)

    distanceBetweenHelper :: Num a => [a] -> [a] -> a -> a
    distanceBetweenHelper [] _ !acc = acc
    distanceBetweenHelper _ [] !acc = acc
    distanceBetweenHelper (x:xs) (y:ys) acc = 
      let newAcc = acc + abs (x - y)
      in distanceBetweenHelper xs ys newAcc

solvePartTwo :: (Ord a, Num a) => [a] -> [a] -> a
-- 1. count the number of occurrences in ys (map from n -> count)
-- 2. for each number in the second list, add that number times its occurrence 
--    in the map above (occurs 0 times by default) to the accumulator
solvePartTwo xs ys = similarityScore xs (fromIntegral 0) $ countOccurrences ys
  where
    countOccurrences :: Ord a => [a] -> Map.Map a Int
    countOccurrences ys = Map.fromListWith (+) [(y, 1) | y <- ys]

    similarityScore :: (Ord a, Num a) => [a] -> a -> Map.Map a Int -> a
    similarityScore [] !acc _  = acc
    similarityScore (x:xs) !acc counts =
      let occurrences = fromIntegral $ Map.findWithDefault 0 x counts
          newAcc = acc + x * occurrences
      in similarityScore xs newAcc counts

solveDayOne :: IO ()
solveDayOne = do
  (xs, ys) <- readFileToLists "data/dayOne.txt"
  print $ solvePartOne xs ys
  print $ solvePartTwo xs ys
  print "solved day one..."

readFileToLists :: FilePath -> IO ([Int], [Int])
readFileToLists filePath = do
  content <- readFile filePath
  let pairs = map parseLine (lines content)
  let col1 = map fst pairs
  let col2 = map snd pairs
  return (col1, col2)

parseLine :: String -> (Int, Int)
parseLine line =
  case words line of
    [x, y] -> (read x, read y)
    _      -> error "Invalid line format"