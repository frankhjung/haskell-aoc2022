{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day03
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( every2nd
             , parse
             , priority
             , solve
             , solve2
             , splitMiddle
             , unique
             , zipOverflow
             ) where

import           Data.Char       (isLower, ord)
import           Data.List       (intersect, nub)
import           Data.List.Split (chunksOf)

-- | Split list into two.
splitMiddle :: [a] -> ([a], [a])
splitMiddle xs = let firstHalf = zipWith const xs (every2nd xs)
                     secondHalf = zipOverflow firstHalf xs
                 in (firstHalf, secondHalf)

-- | Drop every second element.
every2nd :: [a] -> [a]
every2nd (x:_:xs) = x : every2nd xs
every2nd _        = []

-- | Ignore count of first elements then print overflow from second.
zipOverflow :: [a] -> [a] -> [a]
zipOverflow (_:xs) (_:ys) = zipOverflow xs ys
zipOverflow [] ys         = ys -- end of first list so return rest of second
zipOverflow xs []         = xs -- end of second list so return rest of first

-- | Calcualate priority where:
--
-- * Lowercase item types a through z have priorities 1 through 26.
-- * Uppercase item types A through Z have priorities 27 through 52.
priority :: Char -> Int
priority x = if isLower x then ord x - 96 else ord x - 38

-- | Parse input and split into two.
parse :: String -> [(String, String)]
parse = map splitMiddle . lines

score1 :: (String, String) -> Int
score1 (as,bs) = priority $ head $ intersect as bs

-- | Solve puzzle for Day03 - part 1
solve :: String -> Int
solve = sum . map score1 . parse

-- | Select unique elements shared between 3 lists.
unique :: Eq a => [a] -> [a] -> [a] -> [a]
unique as bs cs = intersect (nub as) (nub bs) `intersect` nub cs

parse2 :: String -> [[String]]
parse2 = chunksOf 3 . lines

score2 :: [String] -> Int
score2 (as:bs:cs:_) = priority $ head $ unique as bs cs

-- | Solve puzzle for Day03 - part 2
solve2 :: String -> Int
solve2 = sum . map score2 . parse2
