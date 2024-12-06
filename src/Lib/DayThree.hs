{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}

module Lib.DayThree (solveDayThree) where

import Data.Char
import Control.Applicative

-- a lexer is:
-- a computation producing Maybe (String, a), meaning Lexer Int represents some
-- computation to fulfill before we get the rest of the input and an Int
newtype Lexer a = Lexer
  { runLexer :: String -> Maybe (String, a) 
  }

instance Functor Lexer where
  fmap :: (a -> b) -> Lexer a -> Lexer b
  -- mapping over some computation producing a involves running the computation
  -- and transforming a into b inside a new computation
  fmap f (Lexer l) = Lexer $ \input -> do
    (input', a) <- l input
    return (input', f a)

instance Applicative Lexer where
  pure :: a -> Lexer a
  -- we parse nothing and simply produce a
  pure a = Lexer $ \input -> Just (input, a)

  (<*>) :: Lexer (a -> b) -> Lexer a -> Lexer b
  -- we produce a computation to get b from (a->b) and a
  (Lexer lf) <*> (Lexer la) = Lexer $ \input -> do
    -- run computation to get (a->b)
    (input', f) <- lf input
    -- run computation to get a
    (input'', a) <- la input'
    -- produce b
    return (input'', f a)

instance Alternative Lexer where
  empty :: Lexer a
  -- we probably dont need this?
  empty = Lexer $ const Nothing

  (<|>) :: Lexer a -> Lexer a -> Lexer a
  -- try the first, then try the second
  (Lexer la) <|> (Lexer lb) = Lexer $ \input -> la input <|> lb input

instance Monad Lexer where
  (>>=) :: Lexer a -> (a -> Lexer b) -> Lexer b
  (Lexer l) >>= f = Lexer $ \input -> do
    -- run the first computation
    (input', a) <- l input
    -- build and extract the second computation, apply it to rest of input
    runLexer (f a) input'

char :: Char -> Lexer Char
char c = Lexer $ \input -> case input of
  (x:xs) | x == c -> Just (xs, x)
  _ -> Nothing

string :: String -> Lexer String
string = sequenceA . map char

digit :: Lexer Char
digit = Lexer $ \input -> case input of
  (x:xs) | isDigit x -> Just (xs, x)
  _ -> Nothing

integer :: Lexer Int
integer = do
  digits <- some digit
  return $ read digits

mul :: Lexer Int
mul = do
  -- ignore results explicitly to silence warnings
  _ <- string "mul("
  x <- integer
  _ <- char ','
  y <- integer
  _ <- char ')'
  return (x * y)

-- we need to know the type of result of our lexer to make a decision
data Operation
  = Toggle Bool -- true for do(), false for don't()
  | Multiply Int
  deriving Show

doDont :: Lexer Operation
-- try to parse do() and toggle True, parsing dont() and toggling False if unsuccessful
doDont =
  (string "do()" *> pure (Toggle True)) <|> 
  (string "don't()" *> pure (Toggle False))

mul2 :: Lexer Operation
-- we are interested in 2 things; do/dont and mul (with result wrapped in Multiply)
mul2 = doDont <|> fmap Multiply mul

solvePartOne :: String -> Int
solvePartOne = flip helper 0
  where
    helper [] !acc = acc
    helper input !acc = case runLexer mul input of
      -- tried to parse mul, successfully did, add the product to the accumulator
      Just (input', value) -> helper input' (acc + value)
      -- failed to parse anything, try next character sequence
      Nothing -> helper (tail input) acc

solvePartTwo :: String -> Int
solvePartTwo s = helper 1 s 0
  where
    helper _ [] !acc = acc
    helper toggle input !acc = case runLexer mul2 input of
      -- managed to parse either doDont or mul2...
      -- if we parsed doDont, then we want to adjust our mul toggle
      -- if we just parsed a normal mul2, adding result to the chain depends on the toggle
      Just (input', operation) ->
        case operation of
          Toggle doToggle -> helper (decode doToggle) input' acc
          Multiply value  -> helper toggle input' (acc + toggle*value)
      -- nah we didnt see doDont or mul2, try next character sequence
      Nothing -> helper toggle (tail input) acc   

    decode True = 1
    decode False = 0

solveDayThree :: IO ()
solveDayThree = do
  program <- readFileToProgram "data/dayThree.txt"
  print $ solvePartOne program
  print $ solvePartTwo program
  print "solved day three..."

readFileToProgram :: FilePath -> IO String
readFileToProgram = readFile
