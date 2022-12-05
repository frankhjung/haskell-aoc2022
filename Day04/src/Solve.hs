{-# LANGUAGE OverloadedStrings #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day04
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( contains
             , count
             , fileParser
             , overlaps
             , parseInput
             , Range
             , rangeParser
             , Record (..)
             , recordParser
             , solve
             , solve2
             ) where

import           Control.Monad        (void)
import           Data.Attoparsec.Text (Parser, char, decimal, endOfInput,
                                       endOfLine, many', parseOnly)
import           Data.Either          (fromRight)
import           Data.List            (foldl')
import           Data.Text            (Text, pack)

-- | A Range as a tuple.
type Range = (Int, Int)

-- | Input data
data Record = Record {
                range1 :: Range
              , range2 :: Range} deriving Show

-- | A Range parser.
rangeParser :: Parser Range
rangeParser = do
  r11 <- decimal
  void $ char '-'               -- ignore separator
  r12 <- decimal
  return (r11,r12)

-- | A Record parser.
recordParser :: Parser Record
recordParser = do
  (r11,r12) <- rangeParser
  void $ char ','               -- ignore separator
  (r21,r22) <- rangeParser
  return (Record (r11,r12) (r21,r22))

-- | Parse file input.
fileParser :: Parser [Record]
fileParser = many' (recordParser <* endOfLine)

-- | Parse file content
parseInput :: Text -> Either String [Record]
parseInput = parseOnly (fileParser <* endOfInput)

-- | Does one range contain the other?
contains :: Record -> Bool
contains (Record (a,b) (c,d))
  | a >= c && b <= d = True
  | a <= c && b >= d = True
  | otherwise = False

-- | Does one range overlap the other?
overlaps :: Record -> Bool
overlaps (Record (a,b) (c,d))
  | a <= c && c <= b = True
  | a <= d && d <= b = True
  | c <= a && a <= d = True
  | c <= b && b <= d = True
  | otherwise = False

-- | Count contained ranges.
count :: [Bool] -> Int
count = foldl' (\c t -> if t then succ c else c) 0

-- | Solve puzzle for Day04 - part 1
solve :: String -> Int
solve contents = count contained
  where
    -- get ranges from contents
    records = fromRight [] (parseInput (pack contents))
    -- test if one ranges contains the other
    contained = fmap contains records

-- | Solve puzzle for Day04 - part 2
solve2 :: String -> Int
solve2 contents = count overlapping
  where
    -- get ranges from contents
    records = fromRight [] (parseInput (pack contents))
    -- test if one ranges contains the other
    overlapping = fmap overlaps records
