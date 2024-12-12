module Lib.DayTwelve (solveDayTwelve) where

import qualified Data.Set as Set
import qualified Data.Array as Array

import Data.Array ((!), (//))

type Position = (Int, Int)

type Plot = Array.Array Position Char

type SparsePlot = Array.Array Position (Maybe Char)

-- THE BELOW ALGORITHM IS DONE... MOVE ONTO PART 2

-- for each element in the plot, we check if that element is Nothing, if it is
-- we go next...

-- once we find Just target, we want to do a search on that region, building up
-- a tuple representing (area, inversePerimeter)... we will need to use a visited
-- set to keep track of where we visit, since for each target we land on, we need
-- to count the number of target's in their neighbours... then we need to prune
-- out visited targets from the neighbours, and continue the search... we will add
-- the count of targets in the neighbours to the antiperimeter, before pruning

-- once we have explored an entire region, we produce the area/perimeter combo and 
-- the list of visited positions; we will set all elements in the visited positions 
-- to Nothing, to mark them as skippable/complete for the next iteration...

-- we will build a list of all area/perimeter combos (one for each region), the 
-- visited set is only used to update our search space after we finish checking 
-- out a region...

dfs :: (Position, Char) -> SparsePlot -> ((Int, Int), Set.Set Position)
dfs (startPos, targetChar) sparsePlot = helper startPos Set.empty 0 0
  where
    ((minR, minC), (maxR, maxC)) = Array.bounds sparsePlot

    helper pos visited area anti
      -- we already visited you...
      | pos `Set.member` visited = ((area, anti), visited)
      -- im not sure if i should indent guards with one tab past the condition or
      -- one tab past the guard itself...
      | otherwise = case sparsePlot ! pos of
          -- we have a valid char to explore!
          Just ch | ch == targetChar -> explore visited pos area anti 
          -- neither target or already visited
          _ -> ((area, anti), visited)

    explore visited pos area anti = 
      let validNeighbors = neighbors pos
          selfCount = length $ filter (\n -> sparsePlot ! n == Just targetChar) validNeighbors
      -- keep exploring the neighbours!          
      in processNeighbors validNeighbors (area + 1, anti + selfCount) (Set.insert pos visited)

    -- find neighbours within bounds
    neighbors (r, c) = filter inBounds [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
      where
        inBounds (nr, nc) = nr >= minR && nr <= maxR && nc >= minC && nc <= maxC

    -- we ran out of neighbours, lets produce what we saw
    processNeighbors [] acc visited = (acc, visited)
    -- we have more things to explore! keep building up our region!
    processNeighbors (neighbor:rest) (area, anti) visited =
      let ((newArea, newAnti), newVisited) = helper neighbor visited area anti
      in processNeighbors rest (newArea, newAnti) newVisited

solve1 :: Maybe Position -> SparsePlot -> Int -> Int
-- nothing left to check, we saw the whole grid...
solve1 Nothing sparse totalCost = totalCost
-- we reached an element! check if we visited it before...
solve1 (Just targetPos) sparse totalCost = 
  case sparse ! targetPos of
    -- new char/region to explore! lets find out the area/perimeter and add it
    -- to our collection (aka totalCost)...
    Just ch ->
      let ((area, anti), visited) = dfs (targetPos, ch) sparse
          -- a box has 4 sides, n boxes have n*4 sides, we subtract anti from 
          -- the maximum possible perimeter
          perimeter = area * 4 - anti
          newTotalCost = totalCost + (area*perimeter)
          -- we mark the region we visited as empty now
          newSparse = sparse // [(pos, Nothing) | pos <- Set.toList visited]
      in solve1 next newSparse newTotalCost
    -- we have already visited this region
    Nothing -> solve1 next sparse totalCost
  where
    next = nextPosition targetPos

    nextPosition (r, c) =
      let nextC = (c + 1) `mod` (maxC + 1)
          nextR = r + if nextC == 0 then 1 else 0
      in if nextR > maxR
         then Nothing 
         else Just (nextR, nextC)

    (maxR, maxC) = snd $ Array.bounds sparse

solvePartOne :: Plot -> Int
solvePartOne plot = solve1 (Just (0, 0)) (fmap Just plot) 0

solvePartTwo :: Plot -> Int
solvePartTwo = undefined

solveDayTwelve :: IO ()
solveDayTwelve = do
  plot <- readFileToPlot "data/dayTwelve.txt"
  print $ solvePartOne plot
  print $ solvePartTwo plot
  print "solved day twelve..."

readFileToPlot :: FilePath -> IO Plot
readFileToPlot filePath = do
  content <- readFile filePath
  return $ parsePlot content

parsePlot :: String -> Plot
parsePlot content = 
  let ls = lines content
      rows = length ls
      cols = length (head ls)
  in Array.listArray ((0, 0), (rows-1, cols-1)) (concat ls)