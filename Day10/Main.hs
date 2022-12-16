{-|

Module      : Main
Description : Advent of Code 2022 Day10
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Main (main) where

import           Data.List.Split    (chunksOf)
import           Solve              (solve, solve2)
import           System.Environment (getArgs)

-- Get filename from command line argument.
main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  print (solve contents)
  mapM_ print (chunksOf 40 (solve2 contents))
