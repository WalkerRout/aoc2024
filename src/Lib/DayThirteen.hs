module Lib.DayThirteen (solveDayThirteen) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe, fromJust)

import Control.Monad (guard)

type Point = (Int, Int)

-- a button has an X and a Y offset
data Button = Button Int Int deriving Show

data Equation = Equation
  { buttonA :: Button
  , buttonB :: Button
  , target :: Point
  } deriving (Show)

type Equations = [Equation]

intDiv :: Int -> Int -> Maybe Int
intDiv num denom
  | num `mod` denom == 0 = Just (num `div` denom)
  | otherwise = Nothing

-- rewrite systems of equations in matrix form, we have:
--
-- I * [ A ]  =  [ X ]
--     [ B ]     [ Y ]
--
-- where
--
-- I = [ Ax Bx ]
--     [ Ay By ]
-- det(I) = Ax*By-Ay*Bx
--
-- MA = [ X Bx ]
--      [ Y By ]
-- det(MA) = X*By - Y*Bx
--
-- MB = [ Ax X ]
--      [ Ay Y ]
-- det(MB) = Ax*Y - Ay*X
--
-- and with cramers rule...
--
-- A = det(MA)/det(I)
-- B = det(MB)/det(I)
-- 
solveEquation :: Equation -> Maybe Point
solveEquation (Equation (Button ax ay) (Button bx by) (x, y)) = do
  let detI = ax*by - ay*bx
  guard (detI /= 0)
  let detMA = x*by - y*bx
      detMB = ax*y - ay*x
  a <- detMA `intDiv` detI
  b <- detMB `intDiv` detI
  return $ (a, b)

metric :: (Int, Int) -> Int
metric (a, b) = 3*a + b

solvePartOne :: Equations -> Int
solvePartOne = sum . map metric . mapMaybe solveEquation    

scaleEquation :: Equation -> Equation
-- we define equation as a record, may as well use its features...
scaleEquation e@(Equation _ _ (x, y)) = e { target = (trans x, trans y) }
  where
    trans n = 10000000000000 + n

solvePartTwo :: Equations -> Int
solvePartTwo = sum . map metric . mapMaybe (solveEquation . scaleEquation)

solveDayThirteen :: IO ()
solveDayThirteen = do
  equations <- readFileToEquations "data/dayThirteen.txt"
  print $ solvePartOne equations
  print $ solvePartTwo equations
  print "solved day thirteen..."

readFileToEquations :: FilePath -> IO Equations
readFileToEquations filePath = do
  content <- readFile filePath
  return $ parseEquations content

-- group lines into triples, parsing into equations...
parseEquations :: String -> [Equation]
parseEquations = map parseEquation . groupsOf3 . filter (not . all isSpace) . lines
  where
    groupsOf3 [] = []
    groupsOf3 xs =
      let (a, b) = splitAt 3 xs
      in a : groupsOf3 b

-- parse three lines into an equation
parseEquation :: [String] -> Equation
parseEquation [aLine, bLine, pLine] =
  Equation (parseButtonA aLine) (parseButtonB bLine) (parsePrize pLine)
parseEquation _ = undefined

-- parse a Button A: line
parseButtonA :: String -> Button
parseButtonA line =
  let (x, y) = parseLine "Button A" line
  in Button x y

-- parse a Button B: line
parseButtonB :: String -> Button
parseButtonB line =
  let (x, y) = parseLine "Button B" line
  in Button x y

-- parse a Prize: line
parsePrize :: String -> Point
parsePrize = parseLine "Prize"

-- parse line with known prefix
parseLine :: String -> String -> (Int, Int)
parseLine prefix line =
  -- we want to fail on incorrect input, so we unwrap
  let rest = fromJust $ stripPrefix (prefix ++ ": X") line
      (signXAndXVal, rest') = break (== ',') rest
      (_, xValStr) = splitSign signXAndXVal
      -- ignore ", "
      yPart = drop 2 rest'
      -- unwrap again
      signYAndYVal = fromJust $ stripPrefix "Y" yPart
      (_, yValStr) = splitSign signYAndYVal
      xVal = read xValStr
      yVal = read yValStr
  in (xVal, yVal)

-- split sign from number
-- +29 -> (+, 29)
splitSign :: String -> (Char, String)
splitSign [] = undefined
splitSign (s:xs)
  | s == '+' || s == '=' = (s, xs)
  | otherwise = undefined