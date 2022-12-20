{-|

Module      : Solve
Description : Advent of Code 2022 Day 12
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( allRoutes
             , canClimb
             , getEnd
             , getStart
             , parse
             , setStartEnd
             , solve
             , solve2
             , validMoves
             , Height
             , HeightArray
             , Input
             , Location
             ) where

import           Algorithm.Search (bfs)
import           Control.Monad    (guard)
import           Data.Array       (Array, assocs, bounds, listArray, (!), (//))
import           Data.Char        (ord)
import           Data.Ix          (inRange)
import           Data.List        (find)
import           Data.Maybe       (fromJust)

-- | Our input data type.
type Input = String

-- | Heights are lowercase letters: 'a' - 'z' where 'a' is minimum and 'z'
-- is maximum, 'a' < 'z'.
type Height = Char

-- | Coordinate of latest position.
type Location = (Int, Int)

-- | Grid is an array of heights.
-- type HeightArray = Array Location Height
type HeightArray = Array (Int,Int) Char

-- | Parse input into 2D array.
parse :: Input -> HeightArray
parse raw = listArray ix (concat rows)
  where
    rows = lines raw
    ix = ((0, 0), (length rows - 1, length (head rows) - 1))

-- | Set start and end heights.
setStartEnd :: HeightArray -> (HeightArray, Location, Location)
setStartEnd hs = (hs // [(s, 'a'), (e, 'z')], s, e)
  where
    (s,e) = (getStart hs, getEnd hs)

-- allRoutes :: (HeightArray, Location, Location) -> Int
allRoutes :: (Array (Int, Int) Char, (Int,Int), (Int,Int)) -> Int
allRoutes (hs, start, end) = maybe 0 length $ bfs (validMoves hs) (end ==) start

-- | Return with valid moves from current location.
-- validMoves :: Array (Int,Int) Char -> (Int,Int) -> [(Int,Int)]
validMoves :: HeightArray     -- ^ max bounds (assume starts at (0,0)
              -> Location     -- ^ current location
              -> [Location]   -- ^ neighbours
validMoves hs (x, y) = do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  let
    x' = x + dx
    y' = y + dy
    (mx, my) = snd (bounds hs)
  guard $ dx == 0 || dy == 0  -- no diagonals
  guard $ dx /= dy            -- has moved
  guard $ inRange (0, mx) x'  -- x' in bounds
  guard $ inRange (0, my) y'  -- y' in bounds
  guard $ canClimb (hs!(x,y)) (hs!(x',y')) -- can climb
  return (x',y')

-- | Validation:
-- | neighbour must be no more than 1 unit higher
-- | nightbour has not yet been visited
canClimb :: Char              -- ^ from location
            -> Char           -- ^ to location
            -> Bool           -- True if can climb
canClimb f t = ord t - ord f <= 1

-- | Start index location marked by 'S' (has minimum height 'a').
getStart :: HeightArray -> Location
getStart = fst . fromJust . find ((== 'S') . snd) . assocs

-- | End index location marked by 'E' (has maximum height 'z').
getEnd :: HeightArray -> Location
getEnd = fst . fromJust . find ((== 'E') . snd) . assocs

-- | Solve - part 1 (472)
solve :: Input -> Int
solve = allRoutes . setStartEnd . parse

-- | Solve - part 2
solve2 :: Input -> ()
solve2 = const ()
