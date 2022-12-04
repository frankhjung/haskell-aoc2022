{-|

Module      : Main
Description : Advent of Code 2022 Day04
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Main (main) where

import           Solve              (solve)
import           System.Environment (getArgs)

-- Get filename from command line argument.
main :: IO ()
main = do
  args <- getArgs
  contents <- readFile (head args)
  print (solve contents)
