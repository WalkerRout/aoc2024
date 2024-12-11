{-# LANGUAGE BangPatterns #-}

module Lib.DayNine (solveDayNine) where

import qualified Data.Map as Map
import qualified Data.Array as Array

import Data.List (foldl')
import Data.Char (digitToInt)

-- some empty space somewhere
data Free = Free

-- a file on the tape, with some id
data File = File Int

-- a block of space on a tape...
data Block
  = Open
  | Full Int

-- give a some length
data Span a = Span
  { spanLength :: Int
  , spanBlock  :: a
  }

-- span aliases
type BlockSpan = Span Block
type FreeSpan  = Span Free
type FileSpan  = Span File

-- give a some alignment
data Align a = Align
  { alignStart :: Int
  , alignValue :: a
  }

-- align aliases
type AlignBlock = Align BlockSpan
type AlignFile  = Align FileSpan
type AlignFree  = Align FreeSpan

singleBlockChecksum :: Array.Array Int (Maybe Int) -> Int
singleBlockChecksum arr = helper left right 0
  where
    (left, right) = Array.bounds arr
    -- i=writing, j=reading
    helper !i !j acc
      -- stop when writing head exceeds reading tail
      | i > j = acc
      -- if our writing position is occupied, update our checksum
      | Just val <- arr Array.! i = helper (i + 1) j (acc + i * val)
      -- if j is reading and we are currently empty at i, swap em and go next
      | Just val <- arr Array.! j =
          helper (i + 1) (j - 1) (acc + i * val)
      -- if both positions are invalid, try to update our reading position
      | otherwise = helper i (j - 1) acc

expandTape :: [AlignBlock] -> Array.Array Int (Maybe Int)
-- for efficient indexing, we want to expand this into an array of potential positions
expandTape alignedSpans = Array.listArray (0, totalSize - 1) flatTape
  where
    totalSize = sum [spanLength s | Align _ s <- alignedSpans]
    flatTape = concatMap expandAligned alignedSpans
    expandAligned (Align _ (Span len Open)) = replicate len Nothing
    expandAligned (Align _ (Span len (Full n))) = replicate len (Just n)

solvePartOne :: [AlignBlock] -> Int
solvePartOne = singleBlockChecksum . expandTape

-- we want to operate over aligned files and aligned frees separately
extractFullsAndOpen :: [AlignBlock] -> ([AlignFile], [AlignFree])
extractFullsAndOpen = foldl' processSpan ([], [])
  where
    processSpan (files, free) (Align align (Span size block)) =
      case block of
        Open -> (files, Align align (Span size Free) : free)
        Full fileId -> (Align align (Span size (File fileId)) : files, free)

-- we will move the file, so update our checksum...
checksumOf :: AlignFile -> Int
checksumOf (Align offset (Span size (File fid))) = fid * (2 * offset + size - 1) * size `quot` 2

-- move single file given free list
move :: (Int, Map.Map Int Int) -> AlignFile -> (Int, Map.Map Int Int)
move (acc, freeMap) a@(Align offset s@(Span size _)) =
  -- free blocks out of range dont need to be condsidered
  let free' = Map.takeWhileAntitone (< offset) freeMap
  -- we get the first k,v pair with enough space (v>=size)
  in case [(k, v) | (k, v) <- Map.assocs free', v >= size] of
    -- nowhere had space, end with what we have
    [] -> (acc + checksumOf a, free')
    -- we found somewhere to insert into!
    ((k, v):_) ->
      -- we need to track positions that are completely filled in, since we need to
      -- remove an entry once its filled, otherwise just split the difference between...
      let  free'' | v == size = Map.delete k free'
                  | otherwise = Map.insert (k + size) (v - size) (Map.delete k free')
      in (acc + checksumOf (Align k s), free'')

solvePartTwo :: [AlignBlock] -> Int
solvePartTwo spans = 
  -- we extract all full positions into AlignFile, and all open positions into AlignFree
  let (files, frees) = extractFullsAndOpen spans
      -- naturally we need a way to index into our open slots...
      freeMap = Map.fromList [(alignStart a, spanLength (alignValue a)) | a <- frees]
      (checksum, _) = foldl' move (0, freeMap) files
  in checksum

solveDayNine :: IO ()
solveDayNine = do
  tape <- readFileToTape "data/dayNine.txt"
  print $ solvePartOne tape
  print $ solvePartTwo tape
  print "solved day nine..."

readFileToTape :: FilePath -> IO [AlignBlock]
readFileToTape filePath = do
  content <- readFile filePath
  return $ parseTape content

parseTape :: String -> [AlignBlock]
parseTape = parseChunks 0 0 . map digitToInt
  where
    parseChunks _ _ [] = []
    parseChunks nextId pos (x:xs)
      -- nothing more, lets just add the last file we saw and end
      | null xs = [Align pos (Span x (Full nextId))]
      -- we need to add AlignFile and AlignFree to the list...
      | otherwise =
          Align pos (Span x (Full nextId)) :
          Align (pos + x) (Span (head xs) Open) :
          parseChunks (nextId + 1) (pos + x + head xs) (tail xs)
