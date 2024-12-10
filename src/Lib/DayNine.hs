module Lib.DayNine where

-- decode encoded representation into a list of Maybe Int
-- enumerate all open slots in the tape (any index with Nothing)
-- take enumeration list, and starting from the back of tape, start inserting
-- each element into the positions given by the enumeration list

import Data.Char (ord)

type Index = Int
-- should be an array of id-blocks spaced out
type Tape = [Maybe Int]

-- produce slots where we have nothing...
emptySlots :: Tape -> [Index]
emptySlots tape = zipWith f [0..] tape
  where
    f i tape = 

solvePartOne :: Tape -> Int
solvePartOne = undefined

solveDayNine :: IO ()
solveDayNine = do
  tape <- readFileToEncoded "data/dayNine.txt"
  print $ solvePartOne tape
  print "solved day nine..."

readFileToEncoded :: FilePath -> IO Tape
readFileToEncoded filePath = do
  content <- readFile filePath
  return $ parseTape content 0 []

parseTape :: String -> Int -> Tape -> Tape
parseTape [] _ tape = tape
parseTape (file:free:fs) nextId tape =
  let fileCount = ord file
      freeCount = ord free
      expandedFile = take fileCount . repeat $ Just nextId
      expandedFree = take freeCount . repeat $ Nothing
  in expandedFile ++ expandedFree ++ parseTape fs (nextId + 1) tape
