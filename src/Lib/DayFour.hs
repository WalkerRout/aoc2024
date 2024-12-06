{-# LANGUAGE BangPatterns #-}

module Lib.DayFour (solveDayFour) where

import Data.List (foldl')

import qualified Data.Map as Map

type Grid = [[Char]]
type Position = (Int, Int)
type CachedGrid = Map.Map Position Char

offsets :: [Position]
offsets = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]

createCachedGrid :: Grid -> CachedGrid
createCachedGrid grid =
  Map.fromList [((row, col), ch) | (row, cols) <- zip [0..] grid, (col, ch) <- zip [0..] cols]

generatePositions :: Position -> Position -> Int -> [Position]
generatePositions (row, col) (dRow, dCol) len =
  take len $ iterate (\(r, c) -> (r + dRow, c + dCol)) (row, col)

matchesWord :: (Position -> Maybe Char) -> [Position] -> String -> Bool
-- is the list of positions all True (all chars matched)?
matchesWord gridMap positions word =
  all id $ zipWith match positions word
  where
    match pos ch = gridMap pos == Just ch

countWord :: Grid -> String -> Int
countWord grid word = searchFrom
  where
    positionedGrid = createCachedGrid grid
    gridMap = flip Map.lookup positionedGrid
    wordLength = length word

    countFromPos :: Position -> Int
    countFromPos pos = foldl' f 0 offsets
      where
        f !acc direction =
          let positions = generatePositions pos direction wordLength
              match = if matchesWord gridMap positions word then 1 else 0
          in acc + match

    searchFrom :: Int
    -- iterate over all grid positions, counting each position's word matches
    searchFrom = foldl' f 0 (Map.keys positionedGrid)
      where
        f !acc pos = acc + countFromPos pos

solvePartOne :: Grid -> Int
solvePartOne = flip countWord "XMAS"

countMASX :: Grid -> Int
-- for each A character we land on, check if its in the middle of an X of MAS
countMASX grid = searchFrom
  where
    positionedGrid = createCachedGrid grid
    gridMap = flip Map.lookup positionedGrid

    onA :: Position -> Bool
    onA pos = gridMap pos == Just 'A'

    checkMASX :: Position -> Int
    checkMASX (row, col) =
      let mainDiagonal = ((row-1, col-1), (row+1, col+1))
          antiDiagonal = ((row+1, col-1), (row-1, col+1))
          isMAS (p1, p2) =
            case (gridMap p1, gridMap p2) of
              (Just 'M', Just 'S') -> True
              (Just 'S', Just 'M') -> True
              _ -> False
          isValidXMAS = isMAS mainDiagonal && isMAS antiDiagonal
      in if isValidXMAS then 1 else 0

    searchFrom :: Int
    searchFrom = foldl' f 0 (Map.keys positionedGrid)
      where
        f !acc pos = if onA pos then acc + checkMASX pos else acc

solvePartTwo :: Grid -> Int
solvePartTwo = countMASX

solveDayFour :: IO ()
solveDayFour = do
  grid <- readFileToGrid "data/dayFour.txt"
  print $ solvePartOne grid
  print $ solvePartTwo grid
  print "solved day four..."

readFileToGrid :: FilePath -> IO Grid
readFileToGrid filePath = do
  content <- readFile filePath
  return $ lines content
