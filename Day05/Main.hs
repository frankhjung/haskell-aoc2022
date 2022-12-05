{-|

Module      : Main
Description : Advent of Code 2022 Day05
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Main (main) where

-- import Control.Arrow ((&&&))
-- import           Solve              (solve, solve2)
import           Solve              (solve)
import           System.Environment (getArgs)

-- Get filename from command line argument.
main :: IO ()
main = getArgs >>= readFile . head >>= print . solve
-- main = getArgs >>= readFile . head >>= print . (solve &&& solve2)
