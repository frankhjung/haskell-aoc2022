{-# LANGUAGE OverloadedStrings #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day09
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( parse
             , parseContents
             , lineParser
             , rightParser
             , leftParser
             , upParser
             , downParser
             , move
             , moveBody
             , moveHead
             , moveTail
             , isDiagonal
             , isHorizontal
             , isVertical
             , solve
             , solve2
             , Location
             , Input
             , Position (..)
             , Move
             ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Attoparsec.Text (Parser, decimal, endOfInput, endOfLine,
                                       isEndOfLine, many', parseOnly, skipSpace,
                                       skipWhile, string)
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl')
import           Data.Set             (Set)
import           Data.Set             as S (empty, insert, size)
import           Data.Text            (Text, pack)

type Input = String

-- | Moves from input: Right, Left, Up or Down.
type Move = (Int, Int)

-- | Location on 2D grid.
type Location = (Int, Int)

-- | 2D Grid that we are navigating across.
-- Assume we start at the origin (0,0).
data Position = Position { phead   :: Location      -- ^ head location
                         , ptails  :: [Location]    -- ^ tail location
                         , visited :: Set Location  -- ^ have visited location
                         } deriving (Show, Eq)

-- | == Parsers

rightParser :: Parser [Move]
rightParser = do
  void $ string "R"
  skipSpace
  distance <- decimal
  skipWhile (not . isEndOfLine)
  return (replicate distance (1, 0))

leftParser :: Parser [Move]
leftParser = do
  void $ string "L"
  skipSpace
  distance <- decimal
  skipWhile (not . isEndOfLine)
  return (replicate distance (-1, 0))

upParser :: Parser [Move]
upParser = do
  void $ string "U"
  skipSpace
  distance <- decimal
  skipWhile (not . isEndOfLine)
  return (replicate distance (0, 1))

downParser :: Parser [Move]
downParser = do
  void $ string "D"
  skipSpace
  distance <- decimal
  skipWhile (not . isEndOfLine)
  return (replicate distance (0, -1))

-- | Parse moves from input.
lineParser :: Parser [Move]
lineParser = (rightParser <|> leftParser <|> upParser <|> downParser) <* endOfLine

parseContents :: Text -> Either String [[Move]]
parseContents = parseOnly (many' lineParser <* endOfInput)

parse :: String -> [Move]
parse = concat . fromRight [] . parseContents . pack

-- | Move head. Optionally move tail.
-- First parameter is number of tails.
move :: Int -> [Move] -> Position
move n = foldl' moveBody (Position (0,0) (replicate n (0,0)) S.empty)

-- | Apply move to head and tail.
moveBody :: Position -> Move -> Position
moveBody (Position h ts visits) (n,m) =
  let
    (x,y) = moveHead h (n,m)
    ts' = tail $ scanl moveTail (x,y) ts -- move tails
  in
    Position (x,y) ts' (S.insert (last ts') visits)

-- | Move head once: left, right up or down.
moveHead :: Location        -- ^ head
            -> Location     -- ^ move
            -> Location     -- ^ new head location
moveHead (x,y) (n,m) = (x+n, y+m)

-- | Catchup tail to head.
moveTail :: Location        -- ^ head / previous tail
            -> Location     -- ^ tail
            -> Location     -- ^ new tail location
moveTail (x,y) (v,w)
  | isHorizontal (x,y) (v,w) = (v+v', w   )
  | isVertical   (x,y) (v,w) = (v   , w+w')
  | isDiagonal   (x,y) (v,w) = (v+v', w+w')
  | otherwise                = (v,w)
  where
    v' = signum (x-v)
    w' = signum (y-w)

isHorizontal :: Location -> Location -> Bool
isHorizontal (x,y) (v,w) = abs (x-v) == 2 && y == w

isVertical :: Location -> Location -> Bool
isVertical (x,y) (v,w) = abs (y-w) == 2 && x == v

isDiagonal :: Location -> Location -> Bool
isDiagonal (x,y) (v,w) = abs (x-v) + abs (y-w) >= 3

-- | Solve - part 1 (6057)
solve :: Input -> Int
-- with 1 tail ...
solve = S.size . visited . move 1 . parse

-- | Solve - part 2 (2514)
-- with 9 tails ...
solve2 :: Input -> Int
solve2 = S.size . visited . move 9 . parse
