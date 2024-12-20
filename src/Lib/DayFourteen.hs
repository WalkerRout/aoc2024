{-# LANGUAGE TupleSections #-}

module Lib.DayFourteen (solveDayFourteen) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Control.Monad (guard)
import System.IO (readFile)
import Data.Foldable (toList)

-- Define types for Position and Velocity
type Position = (Int, Int)

type Velocity = (Int, Int)

-- Guard data type with Position and Velocity
data Guard = Guard Position Velocity deriving (Show)

type Guards = [Guard]

-- we can use 2 bits to represent 4 values, so its just a pair of Ordering...
-- one of:
-- GT, GT
-- GT, LT
-- LT, GT
-- LT, LT
type Quadrant = (Ordering, Ordering)

width = 101
height = 103

duplicates :: Ord a => [a] -> [(a, Int)]
duplicates xs = Map.toList $ Map.fromListWith (+) [(x, 1) | x <- xs]

end :: Int -> Guard -> Position
end t (Guard (r, c) (dr, dc)) = ((r + t*dr) `mod` height, (c + t*dc) `mod` width)

-- need to represent nullable elements, so we could use Maybe, but its probably
-- easier to just use an empty list with concat... note this only stores a single
-- element
quadrant :: Position -> [Quadrant]
quadrant (r, c) = do
  let midR = height `div` 2
      midC = width `div` 2
      nr = compare r midR
      nc = compare c midC
  guard (nr /= EQ && nc /= EQ)
  return (nr, nc)

solvePartOne :: Guards -> Int
-- we want to count the number of elements per quadrant
solvePartOne = product . map snd . duplicates . concatMap (quadrant . end 100)

getLargestRobotCluster :: [Position] -> Int -> Int -> Int
-- get position set and find biggest cluster
getLargestRobotCluster positions w h =
    let posSet = Set.fromList positions
    in findLargestCluster posSet w h

findLargestCluster :: Set.Set Position -> Int -> Int -> Int
findLargestCluster posSet w h = helper posSet 0
  where
    helper ps largest
      | Set.null ps = largest
      | otherwise =
          let start = Set.findMin ps
              (clusterSize, visited) = bfsCluster start ps w h
          in helper (ps Set.\\ visited) (max largest clusterSize)

bfsCluster :: Position -> Set.Set Position -> Int -> Int -> (Int, Set.Set Position)
-- never used Seq before, gonna see if i can make it work?
bfsCluster start posSet w h =
  let initialQueue = Seq.singleton start
  in bfs Set.empty initialQueue 0
  where
    -- simple traversal, counting found neighbors...
    bfs visited Seq.Empty size = (size, visited)
    bfs visited (q Seq.:<| qs) size
      | q `Set.member` visited = bfs visited qs size
      | q `Set.member` posSet =
          let visited' = Set.insert q visited
              nbrs = [n | n <- neighbors q w h, n `Set.notMember` visited', n `Set.member` posSet]
          in bfs visited' (qs Seq.>< Seq.fromList nbrs) (size + 1)
      | otherwise = bfs visited qs size

neighbors :: Position -> Int -> Int -> [Position]
-- i dont usually like list comprehensions but i give up...
neighbors (r, c) w h =
  [ (r + dr, c + dc)
  | (dr, dc) <- [(-1,0), (1,0), (0,-1), (0,1)]
  , let nr = r + dr
  , let nc = c + dc
  , nr >= 0, nr < h
  , nc >= 0, nc < w
  ]

solvePartTwo :: Guards -> Int
solvePartTwo guards = search 0
  where
    -- search increments time steps until a large cluster is found
    search t =
      let positions = map (end t) guards
          largestCluster = getLargestRobotCluster positions width height
      in if largestCluster > 15
         then t
         else search (t + 1)

solveDayFourteen :: IO ()
solveDayFourteen = do
  guards <- readFileToGuards "data/dayFourteen.txt"
  print $ solvePartOne guards
  print $ solvePartTwo guards
  print "solved day fourteen..."

readFileToGuards :: FilePath -> IO Guards
readFileToGuards filePath = do
  content <- readFile filePath
  return $ parseGuards content

parseGuards :: String -> Guards
parseGuards = map parseLine . lines

parseLine :: String -> Guard
parseLine line = 
  let [posPart, velPart] = words line
      position = parseTuple (drop 2 posPart)
      velocity = parseTuple (drop 2 velPart)
   in Guard position velocity

parseTuple :: String -> (Int, Int)
parseTuple s =
  let (x, rest) = span (/= ',') s
      y = tail rest
  -- rows on left, cols on right
  in (read y, read x)