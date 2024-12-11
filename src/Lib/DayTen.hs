module Lib.DayTen (solveDayTen) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Array as Array

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
    properHeight n = heightMap Array.! n == height + 1

solvePartOne :: HeightMap -> Int
-- how many different nine height places are reachable from all trailheads?
solvePartOne heightMap =
  let trailheads = [pos | (pos, h) <- Array.assocs heightMap, h == 0]
      -- no duplicates, we use sets...
      reachableSets = map reachableNines trailheads
      -- this still need some cleaning
      mapFrom reached s = Set.foldl' (\m p -> Map.insertWith (+) p 1 m) reached s
      counts = foldl' mapFrom Map.empty reachableSets
  in Map.foldl' (+) 0 counts
  where
    reachableNines start = dfs start (heightMap Array.! start) Set.empty
    dfs pos currentHeight acc
      -- we saw a nine! add it to the pile...
      | currentHeight == 9 = Set.insert pos acc
      | otherwise =
        let nextPositions = validNeighbours pos currentHeight heightMap
        -- just go next man, passing along acc as context...
        in foldl' (\a p -> dfs p (currentHeight + 1) a) acc nextPositions

solvePartTwo :: HeightMap -> Int
-- how many ways can we reach all nine height places?
solvePartTwo heightMap =
  let trailheads = [pos | (pos, h) <- Array.assocs heightMap, h == 0]
      -- we create a map of all nine positions to the number of times we were able
      -- to reach those positions
      finalMap = foldl' scoreTrails Map.empty trailheads
  in Map.foldl' (+) 0 finalMap
  where
    scoreTrails trailMap pos = dfs pos 0 trailMap
    dfs pos currentHeight accMap
      -- we want to see how many times we can reach a given nine from a given start,
      -- so if we are on nine, we better mark it as another hit...
      | currentHeight == 9 = Map.insertWith (+) pos 1 accMap
      -- we should explore our neighbours, and continue looking for nine
      | otherwise =
        let nextPositions = validNeighbours pos currentHeight heightMap
        in foldl' (\m p -> dfs p (currentHeight + 1) m) accMap nextPositions

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