module Lib.DayTwelve (solveDayTwelve) where

import qualified Data.Set as Set
import qualified Data.Array as Array

import Data.Array ((!), (//))

type Position = (Int, Int)

type Plot = Array.Array Position Char

type SparsePlot = Array.Array Position (Maybe Char)

-- aggregates accumulator into cost metric
type Aggregator = (Int, Int) -> Int

-- an explorer takes a position, a target character, a sparse plot, and the current accumulator
-- pair, returning (newAcc, neighbours)
type Explorer = Position -> Char -> SparsePlot -> (Int, Int) -> ((Int, Int), [Position])

solve :: Aggregator -> Explorer -> Maybe Position -> SparsePlot -> Int -> Int
-- nothing left to check, we saw the whole grid...
solve _ _ Nothing _ totalCost = totalCost
-- we reached an element!
solve aggregate explore (Just targetPos) sparse totalCost =
  -- check if we visited the new element before...
  case sparse ! targetPos of
    -- new char/region to explore! lets find out the area/perimeter and add it
    -- to our collection (aka totalCost)...
    Just ch ->
      -- explore subgraph/region of sparse with dfs
      let ((area, metric), visited) = dfs (targetPos, ch)
          costIncrement = aggregate (area, metric)
          newTotalCost = totalCost + costIncrement
          newSparse = sparse // [(pos, Nothing) | pos <- Set.toList visited]
      in solve aggregate explore (nextPosition targetPos) newSparse newTotalCost
    -- we have already visited this region
    Nothing ->
      solve aggregate explore (nextPosition targetPos) sparse totalCost
  where
    (maxR, maxC) = snd $ Array.bounds sparse

    -- help move onto the next search
    nextPosition (r, c) =
      -- we can fit maxC in C, so lets wrap when we exceed max (hence maxC+1)
      let nextC = (c + 1) `mod` (maxC + 1)
          nextR = r + if nextC == 0 then 1 else 0
      -- are we still in the grid?
      in if nextR > maxR
         -- no
         then Nothing
         -- yes, produce coords
         else Just (nextR, nextC)

    dfs (startPos, target) = helper startPos Set.empty (0,0)
      where
        helper pos visited (area, metric)
          -- we already visited you...
          | pos `Set.member` visited = ((area, metric), visited)
          -- im not sure if i should indent guards with one tab past the condition or
          -- one tab past the guard itself...
          | otherwise = case sparse ! pos of
              -- we have a valid char to explore!
              Just ch | ch == target ->
                let ((area', metric'), neighbors) = explore pos ch sparse (area, metric)
                    visited' = Set.insert pos visited
                    -- incredibly creative naming convention
                    ((area'', metric''), visited'') = processNeighbors neighbors visited' (area', metric')
                in ((area'', metric''), visited'')
              -- neither target or already visited
              _ -> ((area, metric), visited)

        -- we ran out of neighbours, lets produce what we saw
        processNeighbors [] visited acc = (acc, visited)
        -- we have more things to explore! keep building up our region!
        processNeighbors (n:ns) visited acc =
          let (acc', visited') = helper n visited acc
          in processNeighbors ns visited' acc'

-- is (r, c) within [(minR, minC), (maxR, maxC)]?
inBounds :: ((Int, Int), (Int, Int)) -> Position -> Bool
inBounds ((minR, minC), (maxR, maxC)) (r, c) =
  r >= minR && r <= maxR && c >= minC && c <= maxC

orthNeighbors :: ((Int, Int), (Int, Int)) -> Position -> [Position]
-- want neighbours directly above/below left/right of (r, c)
orthNeighbors bounds (r, c) = 
  filter (inBounds bounds) [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

aggregateAnti :: Aggregator
aggregateAnti (area, anti) =
  -- a box has 4 sides, n boxes have n*4 sides, we subtract anti from 
  -- the maximum possible perimeter
  let perimeter = area * 4 - anti
  in area * perimeter

exploreAnti :: Explorer
exploreAnti pos ch sparse (area, anti) =
  let bounds = Array.bounds sparse
      neighbors = orthNeighbors bounds pos
      sameRegionNeighbors = filter (\n -> sparse ! n == Just ch) neighbors
      selfCount = length sameRegionNeighbors
  in ((area + 1, anti + selfCount), sameRegionNeighbors)

solvePartOne :: Plot -> Int
-- we want to count the number of interior perimeters and subtract them from the
-- theoretical max, giving us the total perimeter for a region (only orth neighbours)
solvePartOne plot = solve aggregateAnti exploreAnti (Just (0,0)) (fmap Just plot) 0

aggregateCorners :: Aggregator
aggregateCorners (area, sides) = area * sides

exploreCorners :: Explorer
exploreCorners pos ch sparse (area, sides) =
  let bounds = Array.bounds sparse
      cellCorners = countCorners pos
      neighbors = orthNeighbors bounds pos
      sameRegionNeighbors = filter (\n -> sparse ! n == Just ch) neighbors
  in ((area + 1, sides + cellCorners), sameRegionNeighbors)
  where
    -- we want to see 
    countCorners (r, c) =
      let bounds = Array.bounds sparse
          curr = cell bounds (r, c)
          -- we want orthogonal...
          n = cell bounds (r-1, c)
          e = cell bounds (r, c+1)
          s = cell bounds (r+1, c)
          w = cell bounds (r, c-1)
          -- and diagonal neighbours.
          ne = cell bounds (r-1, c+1)
          se = cell bounds (r+1, c+1)
          sw = cell bounds (r+1, c-1)
          nw = cell bounds (r-1, c-1)
          -- are we in the right region?
          same x = x == curr
          -- now we check our corners:
          -- top right C,N,E,Ne
          tr = isCorner (same n, same e, same ne)
          -- bottom right C,S,E,Se
          br = isCorner (same s, same e, same se)
          -- bottom left C,S,W,Sw
          bl = isCorner (same s, same w, same sw)
          -- top left C,N,W,Nw
          tl = isCorner (same n, same w, same nw)
      in tl + tr + br + bl
    
    -- safe access a position in the grid...
    cell bounds pos = if inBounds bounds pos then sparse ! pos else Nothing

    -- inner corners look like:
    -- .!.
    -- .X!
    -- ...
    -- (imagine this 3 more times, rotated)
    -- where two edges are different from the center
    --
    -- outer corners look like:
    -- .X!
    -- .XX
    -- ...
    -- (also this rotated 3 more times)
    -- where a diagonal is different from center and two edges...
    isCorner (orth1Same, orth2Same, diagSame) =
      case (orth1Same, orth2Same, diagSame) of
        -- inner corner: both orth false (different from center)
        (False, False, _) -> 1
        -- Outer corner: both orth true, diag false
        (True, True, False) -> 1
        _ -> 0

solvePartTwo :: Plot -> Int
-- #sides = #corners (look at a drawing of a square), so we can just find the
-- corners in between elements to count the number of different sides...
solvePartTwo plot = solve aggregateCorners exploreCorners (Just (0,0)) (fmap Just plot) 0

solveDayTwelve :: IO ()
solveDayTwelve = do
  plot <- readFileToPlot "data/dayTwelve.txt"
  print $ solvePartOne plot
  print $ solvePartTwo plot
  putStrLn "solved day twelve..."

readFileToPlot :: FilePath -> IO Plot
readFileToPlot filePath = do
  content <- readFile filePath
  return $ parsePlot content

parsePlot :: String -> Plot
parsePlot content = 
  let ls = lines content
      rows = length ls
      cols = length (head ls)
  in Array.listArray ((0,0), (rows-1, cols-1)) (concat ls)
