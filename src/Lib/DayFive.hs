module Lib.DayFive (solveDayFive) where

import Debug.Trace

import qualified Data.Set as Set
import qualified Data.Map as Map

type Rules = Map.Map Int [Int]
type Update = [Int]
type Updates = [Update]

valid :: Rules -> Update -> Bool
-- a page in an update is valid iff none of its associated pages come before it 
-- in the list, so lets 
valid rules update = helper update Set.empty
  where
    helper :: [Int] -> Set.Set Int -> Bool
    -- we see the same shit every time, so all we need to do is track what we seen
    -- and check if anything is within the rule set of the current element
    -- (ie... it was seen -> came before current -> must be invalid)
    helper [] _ = True
    helper (x:xs) seen =
      let associatedPages = Map.findWithDefault [] x rules
          invalid = any (`Set.member` seen) associatedPages
      in not invalid && helper xs (Set.insert x seen)

selectUpdatesWith :: (Rules -> Update -> Bool) -> Rules -> Updates -> Updates
-- todo; use helper with accumulator
selectUpdatesWith _ _ [] = []
selectUpdatesWith f rules (u:us) =
  if f rules u
  then u : selectUpdatesWith f rules us
  else selectUpdatesWith f rules us

center :: [a] -> a
-- center = list[len(list) // 2]
center u = u !! ((length u - 1) `quot` 2)

solvePartOne :: Rules -> Updates -> Int
-- we want to find the center of every valid update, and sum those center elements together...
solvePartOne rules = sum . map center . selectUpdatesWith valid rules

solvePartTwo :: Rules -> Updates -> Int
-- im a monster
solvePartTwo rules = sum . map center . flip reconstruct [] . selectUpdatesWith (\r u -> not $ valid r u) rules
  where
    -- lets destruct updates and try patching current
    reconstruct [] acc = acc
    reconstruct (u:us) acc = reconstruct us (patch u [] : acc)

    -- we can treat each update as completely invalid, reconstructing it from scratch..
    -- this involves taking each x, and trying to place it in a "fixed" accumulator
    patch [] fixed = fixed -- !!! should be `reverse fixed` but we only check the centre... !!!
    patch (x:xs) fixed = patch xs (x `patchInto` fixed)

    -- we need to find a place in fixed where we can place x...
    -- lets build our list *in reverse*, and 'bubble down' the current element
    -- while we are not at the right precedence
    -- todo; use helper with accumulator
    x `patchInto` [] = [x] -- shrimple as
    x `patchInto` fixed@(y:ys)
      -- valid if there isnt anything ahead of us that SHOULD be behind us
      -- (remember, we build the list in reverse, aka X-before-Y becomes Y-before-X here)
      -- if so, place at current spot...
      | all (`notElem` Map.findWithDefault [] x rules) fixed = x : fixed
      -- invalid, place further down...
      | otherwise = y : (x `patchInto` ys)

solveDayFive :: IO ()
solveDayFive = do
  (rules, updates) <- readFileToRules "data/dayFive.txt"
  print $ solvePartOne rules updates
  print $ solvePartTwo rules updates
  print "solved day five..."

readFileToRules :: FilePath -> IO (Rules, Updates)
readFileToRules filePath = do
  content <- readFile filePath
  return $ parseRules content

parseRules :: String -> (Rules, Updates)
parseRules content =
  let (precedenceLines, updateLines) = break null (lines content)
      precedences = foldl insertPrecedence Map.empty (map parsePrecedence precedenceLines)
      updates = map parseUpdate (tail updateLines) -- skip blank line
  in (precedences, updates)

insertPrecedence :: Map.Map Int [Int] -> (Int, Int) -> Map.Map Int [Int]
insertPrecedence acc (key, value) = Map.insertWith (++) key [value] acc

parsePrecedence :: String -> (Int, Int)
parsePrecedence line =
  -- comprehensive checks are for nerds
  let (x, '|':y) = span (/= '|') line
  in (read x, read y)

parseUpdate :: String -> [Int]
parseUpdate line = map read (splitOn ',' line)
  where
    splitOn c s = 
      let (chunk, rest) = break (== c) s
      in case rest of
        []     -> [chunk]
        _:rest -> chunk : splitOn c rest