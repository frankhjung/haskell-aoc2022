{-|

Module      : Solve
Description : Advent of Code 2022 Day06
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( isUnique
             , solve
             , solve2) where

import           Data.List (nub)

type Input = String

-- | isUnique subsequence if first n characters are unique.
isUnique :: Int -> String -> Bool
isUnique m = (m==) . length . nub . take m

-- | Recurise solver.
solve' :: Int         -- sequence window (4 or 14)
          -> Int      -- sequence accumulator
          -> String   -- current working string
          -> Int      -- result
solve' m n xs
  | null xs       = error "expected unique subsequence"
  | isUnique m xs = n + m
  | otherwise     = solve' m (n + 1) (drop 1 xs)

-- | Solve - part 1
solve :: Input -> Int
solve = solve' 4 0

-- | Solve - part 2
solve2 :: Input -> Int
solve2 = solve' 14 0
