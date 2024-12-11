module Lib.DayEight (solveDayEight) where

import Data.List (foldl')

import qualified Data.Set as Set
import qualified Data.Map as Map

type Position = (Int, Int)
-- could be Grid a and Position a if we make a typeclass for euclidean distance...
type Antennas = Map.Map Char [Position]
data Grid = Grid 
  { bounds :: (Int, Int) -- we want to know when to exclude antinodes
  , antennas :: Antennas
  }

-- [a, b)
within :: Ord a => a -> (a, a) -> Bool
within x (xMin, xMax) = x >= xMin && x < xMax

prune :: Position -> (Int, Int) -> Bool
prune (r, c) (rows, cols) = within r (0, rows) && within c (0, cols)

uniquePairs :: [a] -> [(a, a)]
-- a unique pair of nothing has no pairs
uniquePairs [] = []
-- a list of unique pairs is a list of all pairs with the first and the rest of the list, 
-- combined with a list of pairs with the second and the rest of the list, and so on...
uniquePairs (x:xs) = [(x, y) | y <- xs] ++ uniquePairs xs

-- probably should just return a list but we only really need a set
findAntinodes :: ((Position, Position) -> (Int, Int) -> [Position]) -> Grid -> Set.Set Position
findAntinodes antinodes grid =
  let positions = Map.elems $ antennas grid
      -- we generate a big list of every single unique pair of antennas to check
      allPairs = concatMap uniquePairs positions
      -- we find all antinodes for each pair
      allAntinodes = concatMap (flip antinodes (bounds grid)) allPairs
      -- we prune any antinodes that are out of bounds of the grid, in case antinodes function
      -- does not take care of it for us
      pruned = filter (flip prune (bounds grid)) allAntinodes
  in Set.fromList $ pruned

solvePartOne :: Grid -> Int
-- find all antinodes of interest, then count unique positions...
solvePartOne = Set.size . findAntinodes nearbyAntinodes
  where
    nearbyAntinodes ((r1, c1), (r2, c2)) _ =
      let dr = r2 - r1
          dc = c2 - c1
          midR = r1 - dr
          midC = c1 - dc
          farR = r2 + dr
          farC = c2 + dc
      in [(midR, midC), (farR, farC)]

solvePartTwo :: Grid -> Int
solvePartTwo = Set.size . findAntinodes linearAntinodes
  where
    linearAntinodes ((r1, c1), (r2, c2)) (rows, cols) =
      let dr = r2 - r1
          dc = c2 - c1
          -- simplify direction
          gcdVal = gcd dr dc
          stepR = dr `div` gcdVal
          stepC = dc `div` gcdVal
          forwardStep (r, c) = (r + stepR, c + stepC)
          backwardStep (r, c) = (r - stepR, c - stepC)
          -- gen positions in one direction
          forward = takeWhile (flip prune (rows, cols)) $ iterate forwardStep (r2 + stepR, c2 + stepC)
          -- gen positions in other direction
          backward = takeWhile (flip prune (rows, cols)) $ iterate backwardStep (r1 - stepR, c1 - stepC)
      in backward ++ [(r1, c1), (r2, c2)] ++ forward

solveDayEight :: IO ()
solveDayEight = do
  grid <- readFileToGrid "data/dayEight.txt"
  print $ solvePartOne grid
  print $ solvePartTwo grid
  print "solved day eight..."

readFileToGrid :: FilePath -> IO Grid
readFileToGrid filePath = do
  content <- readFile filePath
  return $ parseGrid content

parseGrid :: String -> Grid
-- for each char in the grid,
parseGrid content = 
  let rows = lines content
      bounds = (length rows, length . head $ rows)
      antennas = foldl' (flip parseRowInto) Map.empty (zip [0..] rows)
  in Grid bounds antennas

-- parse (rowNum, row) into some antenna map
parseRowInto :: (Int, String) -> Antennas -> Antennas
parseRowInto (r, row) grid = foldl' (flip parseCharInto) grid (zip [0..] indexedChars)
  where
    indexedChars = map (\ch -> (r, ch)) row

-- (colNum, (rowNum, char))
parseCharInto :: (Int, (Int, Char)) -> Antennas -> Antennas
parseCharInto (_, (_, '.')) grid = grid
parseCharInto (c, (r, ch))  grid = Map.insertWith (++) ch [(r, c)] grid