module Lib.DaySeven (solveDaySeven) where

import Data.Int (Int64)
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Maybe (catMaybes)

import Control.Applicative

-- an equation we are attempting to construct, contains a result and list of operands
data ProtoEquation = ProtoEquation Int64 [Int64]

-- take/skip problem, keep searching for paths and pruning ones that dont work out
-- we will recursively compare the application of each function to our current result
-- and the next operand in the list; if our current result is equal to the actual 
-- result, produce that... if we cant find a result, produce Nothing
identifyOperators :: ProtoEquation -> [(Int64 -> Int64 -> Int64)] -> Maybe Int64
-- we take the initial number out of the operands, since we dont know if we will be 
-- multiplying (acc needs to be 1) or adding (acc needs to be 0) in the first operation
identifyOperators (ProtoEquation result (initial:rest)) fs = helper rest initial
  where
    -- todo; memoize this function...
    helper [] acc 
      -- somehow we found the right path
      | acc == result = Just acc
      -- missed it...
      | otherwise = Nothing
    -- for each operand in the list, we want to test out each path, dictated by
    -- each function f <- fs
    helper (o:os) acc =
      -- we want to combine the result of f guiding the path with all other paths guided
      -- by the other fs
      let eitherOr found f = helper os (f acc o) <|> found
      in foldl' eitherOr Nothing fs
-- someone passed in <2 operands...
identifyOperators _ _ = Nothing

mulOrAdd :: ProtoEquation -> Maybe Int64
mulOrAdd = flip identifyOperators [(*), (+)]

solvePartOne :: [ProtoEquation] -> Int64
solvePartOne = sum . catMaybes . map mulOrAdd

digits :: Int64 -> Int64
digits 0 = 1
digits n =
  let x = abs n
  in floor (logBase 10 (fromIntegral x)) + 1

combine :: Int64 -> Int64 -> Int64
-- shift x over by the number of digits in y, then place y in the newly open space
combine x y = x * (10 ^ (digits y)) + y

mulAddOrConcat :: ProtoEquation -> Maybe Int64
mulAddOrConcat = flip identifyOperators [(*), (+), combine]

solvePartTwo :: [ProtoEquation] -> Int64
solvePartTwo = sum . catMaybes . map mulAddOrConcat

solveDaySeven :: IO ()
solveDaySeven = do
  equations <- readFileToEquations "data/daySeven.txt"
  print $ solvePartOne equations
  print $ solvePartTwo equations
  print "solved day seven..."

readFileToEquations :: FilePath -> IO [ProtoEquation]
readFileToEquations filePath = do
  content <- readFile filePath
  return $ parseEquations content

parseEquations :: String -> [ProtoEquation]
parseEquations content = map parseEquation $ lines content

parseEquation :: String -> ProtoEquation
parseEquation line =
  let (resPart, opsPart) = break (== ':') line
      res = read (trim resPart)
      ops = map read (words (drop 1 (dropWhile (/= ' ') opsPart)))
  in ProtoEquation res ops

trim :: String -> String
-- im fuckin amazed this isnt in the standard library; ive pretty much stolen the
-- impl. from https://hackage.haskell.org/package/extra-1.8/docs/src/Data.List.Extra.html#trim
trim = dropWhile isSpace . dropWhileEnd isSpace
  where
    dropWhileEnd :: (Char -> Bool) -> String -> String
    dropWhileEnd _ [] = []
    dropWhileEnd p (x:xs) =
      let rest = dropWhileEnd p xs
      in if null rest && p x
         then []
         else x : rest