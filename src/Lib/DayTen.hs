module Lib.DayTen (solveDayTen) where

import qualified Data.Map as Map
import qualified Data.Array as Array

import Data.Array ((!))

import Data.List (foldl')
import Data.Char (digitToInt)

type Position = (Int, Int)

type HeightMap = Array.Array Position Int

neighbors :: Position -> HeightMap -> [Position]
neighbors (r, c) heightMap =
  let ((minR, minC), (maxR, maxC)) = Array.bounds heightMap
      -- north/east/south/west
      potentials = [(r-1, c), (r, c-1), (r+1, c), (r, c+1)]
      inBounds (x, y) = x >= minR && x <= maxR && y >= minC && y <= maxC
  in filter inBounds potentials

validNeighbours :: Position -> Int -> HeightMap -> [Position]
validNeighbours pos height heightMap = filter properHeight $ neighbors pos heightMap
  where
    properHeight n = heightMap ! n == height + 1

-- we need a way to aggregate all 9 height places for a single trailhead
pathsFrom :: HeightMap -> Position -> Map.Map Position Int
-- for a given starting position at 0, find all unique, reachable 9 heights, counting
-- the number of distinct paths that led to said position
pathsFrom heightMap start = foldl' (step heightMap) (Map.singleton start 1) [1..9]

step :: HeightMap -> Map.Map Position Int -> Int -> Map.Map Position Int
-- given current reachable positions (and their path counts) at some height h-1,
-- move to all positions of height h
step heightMap currentMap targetHeight =
  Map.fromListWith (+)
    [ (newPos, count)
    | (pos, count) <- Map.assocs currentMap
    , newPos <- validNeighbours pos (targetHeight - 1) heightMap
    ]

solvePartOne :: HeightMap -> Int
-- how many distinct 9 height places are reachable from all trailheads?
-- sum up how many distinct locations are reachable (per trailhead)
solvePartOne heightMap =
  let trailheads = [pos | (pos, h) <- Array.assocs heightMap, h == 0]
      allPaths = map (pathsFrom heightMap) trailheads
  in sum (map length allPaths)

solvePartTwo :: HeightMap -> Int
-- how many total paths reach 9 height places from trailheads?
-- just sum up counts of all 9s from reachable trailheads
solvePartTwo heightMap =
  let trailheads = [pos | (pos, h) <- Array.assocs heightMap, h == 0]
      allPaths = map (pathsFrom heightMap) trailheads
  in sum (map sum allPaths)

solveDayTen :: IO ()
solveDayTen = do
  heightMap <- readFileToHeightMap "data/dayTen.txt"
  print $ solvePartOne heightMap
  print $ solvePartTwo heightMap
  print "solved day ten..."

readFileToHeightMap :: FilePath -> IO HeightMap
readFileToHeightMap filePath = do
  content <- readFile filePath
  return $ parseHeightMap content

parseHeightMap :: String -> HeightMap
parseHeightMap content = 
  let linesOfContent = lines content
      numRows = length linesOfContent
      numCols = if numRows > 0 then length (head linesOfContent) else 0
      -- put everything back, this time as integers
      elems = concatMap (map digitToInt) linesOfContent
      bounds = ((0, 0), (numRows-1, numCols-1))
  -- create some nxm 2d array from nxm elements in a list
  in Array.listArray bounds elems