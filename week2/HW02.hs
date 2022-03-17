{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches actualPegs guessPegs =
    length $ filter foundMatch $ zip actualPegs guessPegs where 
        foundMatch :: (Peg, Peg) -> Bool
        foundMatch (actual, guess) = actual == guess


-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors pegs = map countOccurrences colors where
    countOccurrences :: Peg -> Int
    countOccurrences color = length $ filter (\peg -> peg == color) pegs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actualPegs guessPegs = 
    sum $ map (\(x, y) -> min x y) $ zip actualColors guessColors where
            actualColors = countColors actualPegs
            guessColors = countColors guessPegs


-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actualPegs guessPegs =
    let moveExactMatches = exactMatches actualPegs guessPegs in
    Move guessPegs moveExactMatches ((matches actualPegs guessPegs) - moveExactMatches)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move moveCode moveExactMatches moveNonExactMatches) code =
    let newExactMatches = exactMatches moveCode code in
    newExactMatches == moveExactMatches &&
        (matches moveCode code) - newExactMatches == moveNonExactMatches

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
