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
             , rParser
             , lParser
             , uParser
             , dParser
             , move
             , moveHeadTail
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
                                       skipWhile, string, try)
import           Data.Either          (fromRight)
import           Data.Foldable        (foldl')
import           Data.Set             (Set)
import           Data.Set             as S (empty, insert, size)
import           Data.Text            (Text, pack)
import           Prelude              hiding (Left, Right)

type Input = String

-- | Moves from input: Right, Left, Up or Down.
type Move = (Int, Int)

-- | Location on 2D grid.
type Location = (Int, Int)

-- | 2D Grid that we are navigating across.
-- Assume we start at the origin (0,0).
data Position = Position { phead   :: Location        -- ^ elfs head location
                         , ptail   :: Location        -- ^ elfs tail location
                         , visited :: Set Location  -- ^ have visited location
                         } deriving (Show, Eq)

-- | == Parsers

rParser :: Parser [Move]
rParser = do
  void $ string "R"
  skipSpace
  distance <- decimal
  skipWhile (not . isEndOfLine)
  return (replicate distance (1, 0))

lParser :: Parser [Move]
lParser = do
  void $ string "L"
  skipSpace
  distance <- decimal
  skipWhile (not . isEndOfLine)
  return (replicate distance (-1, 0))

uParser :: Parser [Move]
uParser = do
  void $ string "U"
  skipSpace
  distance <- decimal
  skipWhile (not . isEndOfLine)
  return (replicate distance (0, 1))

dParser :: Parser [Move]
dParser = do
  void $ string "D"
  skipSpace
  distance <- decimal
  skipWhile (not . isEndOfLine)
  return (replicate distance (0, -1))

-- | Parse moves from input.
lineParser :: Parser [Move]
lineParser = (try rParser <|> lParser <|> uParser <|> dParser) <* endOfLine

parseContents :: Text -> Either String [[Move]]
parseContents = parseOnly (many' lineParser <* endOfInput)

parse :: String -> [Move]
parse = concat . fromRight [] . parseContents . pack

-- -- | Move head. Optionally move tail.
move :: [Move] -> Position
move = foldl' moveHeadTail (Position (0,0) (0,0) S.empty)

-- -- | Apply move to head and tail.
moveHeadTail :: Position -> Move -> Position
moveHeadTail (Position h t visits) (n,m) =
  let
    (x,y) = moveHead h (n,m)
    (v,w) = moveTail t (x,y)
  in
    Position (x,y) (v,w) (S.insert (v,w) visits)

-- | Move head once: left, right up or down.
moveHead :: Location        -- ^ head
            -> Location     -- ^ move
            -> Location     -- ^ new head location
moveHead (x,y) (n,m) = (x+n, y+m)

-- | Catchup tail to head.
moveTail :: Location        -- ^ tail
            -> Location     -- ^ head
            -> Location     -- ^ new tail location
moveTail (v,w) (x,y)
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
isDiagonal (x,y) (v,w) = abs (x-v) + abs (y-w) == 3

-- | Solve - part 1 (6057)
solve :: Input -> Int
solve = S.size . visited . move . parse

-- | Solve - part 2
solve2 :: Input -> ()
solve2 = const ()
