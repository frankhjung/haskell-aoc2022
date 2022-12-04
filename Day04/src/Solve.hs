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
             , parseInput
             , Range
             , rangeParser
             , Record (..)
             , recordParser
             , solve
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
contains :: Range -> Range -> Bool
contains (a,b) (c,d)
  | a >= c && b <= d = True
  | a <= c && b >= d = True
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
    contained = fmap (\(Record r1 r2) -> contains r1 r2) records
