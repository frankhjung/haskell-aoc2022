{-|

Module      : Solve
Description : Advent of Code 2022 Day04
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( solve
             , solve2) where

-- | Solve puzzle for Day04 - part 1
solve :: String -> Int
solve = sum . map read . lines

-- | Solve puzzle for Day04 - part 2
solve2 :: String -> Int
solve2 = sum . map read . lines
