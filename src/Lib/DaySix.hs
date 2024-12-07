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

countUniquePositions :: Grid -> Guard -> Int
-- check in front of us; if we have nothing infront of us, stop
-- if we dont, try moving guard
countUniquePositions grid guard = helper guard Set.empty
  where
    helper !guard@(Guard pos _) seen = case viewFront grid guard of
      Nothing -> Set.size newSeen
      Just infront -> helper (advance guard infront) newSeen
      where
        newSeen = Set.insert pos seen

solvePartOne :: Grid -> Guard -> Int
solvePartOne grid guard = countUniquePositions grid guard

simulateLoop :: Grid -> Guard -> Bool
simulateLoop grid guard = helper guard Set.empty
  where
    helper !guard seen
      | guard `Set.member` seen = True
      | otherwise = case viewFront grid guard of
          Nothing -> False
          Just infront -> 
            let newGuard = advance guard infront
            in helper newGuard (Set.insert guard seen)

causesLoop :: Grid -> Guard -> Position -> Bool
causesLoop grid guard pos =
  -- replace whatever with Obstacle at pos
  let modifiedGrid = Map.adjust (const Obstacle) pos grid
  in simulateLoop modifiedGrid guard

countLoopCausingObstacles :: Grid -> Guard -> Int
-- for every open position, place an obstacle and simulate the guards path
countLoopCausingObstacles grid guard@(Guard start _) =
  let candidates = [pos | (pos, cell) <- Map.toList grid, cell == Empty, pos /= start]
  in helper candidates 0
  where
    helper [] !acc = acc
    helper (c:cs) !acc = 
      let newAcc = if causesLoop grid guard c then acc+1 else acc
      in helper cs newAcc

solvePartTwo :: Grid -> Guard -> Int
solvePartTwo grid guard = countLoopCausingObstacles grid guard

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