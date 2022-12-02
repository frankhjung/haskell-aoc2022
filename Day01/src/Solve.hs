{-|

Module      : Solve
Description : Advent of Code 2022 Day01
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( solve
             , solve2) where

import           Data.List       (sortOn)
import           Data.List.Split (splitWhen)
import           Data.Ord        (Down (..))

-- | Helper function to summarise calories carried by each elf.
calories :: String -> [Int]
calories = map (sum . map read) . splitWhen ("" ==) . lines

-- | Solve puzzle for Day01 - part 1
-- Sum calories for top elf.
solve :: String -> Int
solve = maximum . calories

-- | Solve puzzle for Day01 - part 2
-- Sum calories for top 3 elves.
solve2 :: String -> Int
solve2 = sum . take 3 . sortOn Down . calories
