{-# LANGUAGE BangPatterns #-}

module Lib.DaySix (solveDaySix) where

import Control.Applicative

import qualified Data.Set as Set
import qualified Data.Map as Map

type Position = (Int, Int)
data Direction = North | East | South | West deriving (Ord, Eq)
data Guard = Guard Position Direction deriving (Ord, Eq)
data Cell = Obstacle | Empty deriving Eq
type Grid = Map.Map Position Cell

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

advance :: Guard -> Cell -> Guard
-- if we have obstacle, turn guard right
-- if we have empty, move guard forward one
advance (Guard pos dir) Obstacle = Guard pos (turnRight dir)
advance guard@(Guard _ dir) Empty = Guard (march guard) dir

march :: Guard -> Position
march (Guard (r, c) North) = (r-1, c)
march (Guard (r, c) East)  = (r, c+1)
march (Guard (r, c) South) = (r+1, c)
march (Guard (r, c) West)  = (r, c-1)

viewFront :: Grid -> Guard -> Maybe Cell
viewFront grid guard = Map.lookup (march guard) grid

visitedPositions :: Grid -> Guard -> Set.Set Position
visitedPositions grid guard = helper guard Set.empty
  where
    helper !guard@(Guard pos _) seen = case viewFront grid guard of
      Nothing -> Set.insert pos seen
      Just infront -> helper (advance guard infront) (Set.insert pos seen)

countUniquePositions :: Grid -> Guard -> Int
countUniquePositions grid = Set.size . visitedPositions grid

solvePartOne :: Grid -> Guard -> Int
solvePartOne = countUniquePositions

causesLoop :: Grid -> Guard -> Bool
causesLoop grid guard = helper guard Set.empty
  where
    helper !guard seen
      | guard `Set.member` seen = True
      | otherwise = case viewFront grid guard of
          Nothing -> False
          Just infront -> 
            let newGuard = advance guard infront
            in helper newGuard (Set.insert guard seen)

countLoopCausingObstacles :: Grid -> Guard -> Int
countLoopCausingObstacles grid guard@(Guard start _) =
  let visited = visitedPositions grid guard
      candidates = Set.toList $ Set.filter (/= start) visited
  in helper candidates 0
  where
    helper [] !acc = acc
    -- for each candidate c, we check if adding an obstacle on c causes a loop,
    -- if it does, we increment the number of loops caused, otherwise we continue to next
    helper (c:cs) !acc = 
      let modifiedGrid = Map.adjust (const Obstacle) c grid
          newAcc = if causesLoop modifiedGrid guard then acc+1 else acc
      in helper cs newAcc

solvePartTwo :: Grid -> Guard -> Int
solvePartTwo = countLoopCausingObstacles

solveDaySix :: IO ()
solveDaySix = do
  (grid, guard) <- readFileToLab "data/daySix.txt"
  print $ solvePartOne grid guard
  print $ solvePartTwo grid guard
  print "solved day six..."

readFileToLab :: FilePath -> IO (Grid, Guard)
readFileToLab filePath = do
  content <- readFile filePath
  return $ parseLab content

parseLab :: String -> (Grid, Guard)
parseLab content = parseRows (lines content) 0 Map.empty Nothing

parseRows :: [[Char]] -> Int -> Grid -> Maybe Guard -> (Grid, Guard)
parseRows [] _ grid (Just guard) = (grid, guard)
-- this case is not possible, we must have a guard in the input
parseRows [] _ _ Nothing = undefined
parseRows (row:rows) rowIndex grid guard =
  let (updatedGrid, updatedGuard) = parseRow row (rowIndex, 0) grid guard
  in parseRows rows (rowIndex + 1) updatedGrid updatedGuard

parseRow :: [Char] -> Position -> Grid -> Maybe Guard -> (Grid, Maybe Guard)
parseRow [] _ grid guard = (grid, guard)
parseRow (c:cs) pos@(row, col) grid guard =
  let (cell, newGuard) = parseCell c pos
      updatedGrid = Map.insert pos cell grid
      updatedGuard = guard <|> newGuard
  in parseRow cs (row, col + 1) updatedGrid updatedGuard

parseCell :: Char -> Position -> (Cell, Maybe Guard)
parseCell c pos = case c of
  '#' -> (Obstacle, Nothing)
  '.' -> (Empty, Nothing)
  '^' -> (Empty, Just (Guard pos North))
  '>' -> (Empty, Just (Guard pos East))
  'v' -> (Empty, Just (Guard pos South))
  '<' -> (Empty, Just (Guard pos West))
  -- this case is not possible, we dont accept other chars as input
  _   -> undefined