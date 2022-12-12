{-|

Module      : Solve
Description : Advent of Code 2022 Day09
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( solve
             , solve2) where

type Input = String

-- | Solve - part 1
solve :: Input -> Int
solve = sum . map read . lines

-- | Solve - part 2
solve2 :: Input -> ()
solve2 = const ()
