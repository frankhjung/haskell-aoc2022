{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day02
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( cpred
             , csucc
             , parse
             , play
             , play2
             , result
             , score
             , score1
             , solve
             , solve2
             , turn
             , Result (..)
             , Turn (..)
             ) where

-- | Cyclic enumerator.
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred c
    | c == minBound = maxBound
    | otherwise = pred c
  csucc :: a -> a
  csucc c
    | c == maxBound = minBound
    | otherwise = succ c

-- | Data type to manage Rock, Paper, Scissors.
data Turn = Rock | Paper | Scissors deriving (Show, Eq, Enum, Bounded)

-- | Make Turns cyclic enumerator.
instance CyclicEnum Turn

-- | Make a Turn from coded string input.
instance Read Turn where
  readsPrec _ str = [(mkTurn str, "")]

mkTurn :: String -> Turn
mkTurn str = case str of
             "A" -> Rock
             "B" -> Paper
             "C" -> Scissors
             "X" -> Rock
             "Y" -> Paper
             "Z" -> Scissors
             _   -> error ("no parse" ++ str)

-- | Score a turn
turn :: Turn -> Int
turn Rock     = 1
turn Paper    = 2
turn Scissors = 3

-- | Result of a turn.
data Result = Lose | Draw | Win deriving (Show, Eq)

-- | Make a Result from coded string input.
instance Read Result where
  readsPrec _ str = [(mkResult str, "")]

mkResult :: String -> Result
mkResult str = case str of
             "X" -> Lose
             "Y" -> Draw
             "Z" -> Win
             _   -> error ("no parse" ++ str)

-- | Score a result
result :: Result -> Int
result Lose = 0
result Draw = 3
result Win  = 6

-- | Evaluate the game final score: turn score + game outcome
score :: Turn -> Turn -> Int
score a b = result (play a b) + turn b

-- | Parse string input to list of turns.
parse :: String -> [[String]]
parse content = map words (lines content)

-- | == Part 1

-- | Play a turn part 1
play :: Turn -> Turn -> Result
play a b
  | a == b = Draw
  | a == csucc b = Lose
  | a == cpred b = Win

-- | Score part 1
score1 :: [String] -> Int
score1 (a:x:_) = score (mkTurn a) (mkTurn x)

-- | Solve part 1
solve :: String -> Int
solve = sum . map score1 . parse

-- | == Part 2

-- | Play a turn part 2
play2 :: Turn -> Result -> Turn
play2 a x
  | x == Draw = a
  | x == Lose = cpred a
  | x == Win  = csucc a

-- | Score part 2
score2 :: [String] -> Int
score2 (a:x:_) =
  let
    ta = mkTurn a
    tx = play2 ta (mkResult x)
  in score ta tx

-- | Solve part 2
solve2 :: String -> Int
solve2 = sum . map score2 . parse
