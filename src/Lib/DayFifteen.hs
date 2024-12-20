module Lib.DayFifteen (solveDayFifteen) where

import Debug.Trace

import qualified Data.Map as Map

import Data.List (foldl')
import Data.Maybe (fromJust)

type Position = (Int, Int)

-- 0, ., #
data Cell = Box | Open | Wall deriving (Show, Eq)

type Warehouse = Map.Map Position Cell

data Grid = Grid
  { current   :: Position
  , warehouse :: Warehouse
  } deriving (Show)

data Direction = North | East | South | West deriving (Show, Eq)

type Directions = [Direction]

charToDir :: Char -> Direction
charToDir '^' = North
charToDir '>' = East
charToDir 'v' = South
charToDir '<' = West
charToDir _ = undefined

-- probably shouldnt be mixing up canadian/american words all the time but idc
neighbour :: Position -> Direction -> Position
neighbour (r, c) North = (r-1, c)
neighbour (r, c) East  = (r, c+1)
neighbour (r, c) South = (r+1, c)
neighbour (r, c) West  = (r, c-1)

move :: Direction -> Grid -> Grid
-- we move in a direction by consuming a direction and our current state,
-- trying to update the current position and *any aligned boxes in the way*
-- to align boxes, we check the neighbour in the direction we are moving,
-- if its a box, we keep going in that direction until we reach an open spot,
-- to which we can finally update all positions in the line of aggregated boxes...
-- unless we run into a wall, which will cause our computation to fail, since we
-- cannot move any boxes...
-- Main move function
move direction grid@(Grid curr warehouse) =
  let next = neighbour curr direction
  in case Map.lookup next warehouse of
    Just Box -> 
      case canMoveBoxes direction next warehouse of
        Just warehouse' -> Grid next warehouse'
        Nothing -> grid
    Just Open -> grid { current = next }
    _ -> grid

-- Recursive check and update for boxes
canMoveBoxes :: Direction -> Position -> Warehouse -> Maybe Warehouse
canMoveBoxes direction pos warehouse =
  let next = neighbour pos direction
  in case Map.lookup next warehouse of
    Just Box -> do
      -- recursively try to move the next box
      warehouse' <- canMoveBoxes direction next warehouse
      -- replace and move box
      let warehouse'' = Map.insert next Box (Map.insert pos Open warehouse')
      return $ warehouse''
    Just Open -> Just $ Map.insert next Box (Map.insert pos Open warehouse)
    _ -> Nothing

sumGPSCoordinates :: Warehouse -> Int
sumGPSCoordinates warehouse =
  let isBox (_, cell) = cell == Box
      boxPositions = map fst $ filter isBox (Map.toList warehouse)
      gps (r, c) = 100 * r + c
  in sum $ map gps boxPositions

solvePartOne :: Grid -> Directions -> Int
-- for each direction in directions, we want to try to update the grid given current
-- position and said direction...
solvePartOne grid directions = fst $ current $ foldl' (flip move) grid directions

solveDayFifteen :: IO ()
solveDayFifteen = do
  (grid, directions) <- readFileToWarehouseDirections "data/dayFifteen.txt"
  print $ solvePartOne grid directions
  print "solved day fifteen..."

readFileToWarehouseDirections :: FilePath -> IO (Grid, Directions)
readFileToWarehouseDirections filePath = do
  content <- readFile filePath
  return $ parseBoth content

parseBoth :: String -> (Grid, Directions)
parseBoth content =
  let (gridLines, directionLines) = break null (lines content)
      grid = parseGrid gridLines 0 (Grid (0, 0) Map.empty)
      directions = parseDirections (drop 1 directionLines)
  in (grid, directions)

parseGrid :: [String] -> Int -> Grid -> Grid
parseGrid [] _ grid = grid
parseGrid (row:rows) rowIndex grid =
  let updatedGrid = parseRow row (rowIndex, 0) grid
  in parseGrid rows (rowIndex + 1) updatedGrid

parseRow :: String -> Position -> Grid -> Grid
parseRow [] _ grid = grid
parseRow (c:cs) pos@(row, col) (Grid currentPos warehouse) =
  let newGrid = case parseCell c pos of
        -- we found new position instead of a valid cell, so we must take care of
        -- replacing with a new Open cell...
        Left newPos -> Grid newPos (Map.insert newPos Open warehouse)
        Right cell  -> Grid currentPos (Map.insert pos cell warehouse)
  in parseRow cs (row, col + 1) newGrid

parseCell :: Char -> Position -> Either Position Cell
parseCell '@' pos = Left pos
parseCell 'O' _   = Right Box
parseCell '.' _   = Right Open
parseCell '#' _   = Right Wall
parseCell _ _ = undefined

parseDirections :: [String] -> Directions
parseDirections = map charToDir . concat