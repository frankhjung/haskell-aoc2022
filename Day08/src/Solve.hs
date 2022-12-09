{-|

Module      : Solve
Description : Advent of Code 2022 Day08
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( count
             , countInner
             , countPerimeter
             , isLeft
             , isRight
             , isTop
             , isBot
             , parse
             , perimeter
             , solve
             , solve2) where

-- import           Control.Monad (ap)
import           Data.Array (Array, bounds, listArray, (!))
import           Data.Char  (digitToInt)
import           Data.Ix    (range)

type Input = String
type Row = Int
type Col = Int

-- | Check if tree visible from the left.
isLeft :: (Row,Col) -> Array (Int,Int) Int -> Bool
isLeft (r,c) as = sum [1 | i<-range(0,c-1), as!(r,c) > as!(r,i)] == c

-- | Check if tree visible from the right.
isRight :: (Row,Col) -> Array (Int,Int) Int -> Bool
isRight (r,c) as = sum [1 | i<-range(c+1,n), as!(r,c) > as!(r,i)] == n - c
  where ((_,_),(n,_)) = bounds as

-- | Check if tree visible from the top.
isTop :: (Row,Col) -> Array (Int,Int) Int -> Bool
isTop (r,c) as = sum [1 | i<-range(0,r-1), as!(r,c) > as!(i,c)] == r

-- | Check if tree visible from the bottom.
isBot :: (Row,Col) -> Array (Int,Int) Int -> Bool
isBot (r,c) as = sum [1 | i<-range(r+1,n), as!(r,c) > as!(i,c)] == n - r
  where ((_,_),(n,_)) = bounds as

perimeter :: Array (Int,Int) Int -> Int
perimeter as = r + c + 1
    where ((r,_),(c,_)) = bounds as

-- | Count perimeter.
countPerimeter :: Array (Int,Int) Int -> Int
countPerimeter = subtract 4 . (4 *) . perimeter

-- | Count of visible trees.
countInner :: Array (Int,Int) Int -> Int
countInner as = length $ filter (==True) visible
  where
    ((s,_),(e,_)) = bounds as -- outer bounds
    (s',e') = (s+1,e-1) -- inner bounds
    visible = [or [isLeft (r,c) as,isRight (r,c) as, isTop (r,c) as, isBot (r,c) as] | r <- range(s',e'), c <- range(s',e')]

-- | Count visible trees and perimeter.
count :: Array (Int,Int) Int -> Int
count as = countPerimeter as + countInner as

-- | Parse line contents to list of list of integers.
parse :: Input -> Array (Int,Int) Int
parse ss = listArray ((0,0),(n,n)) (concatMap (map digitToInt) ss')
  where
    ss' = lines ss
    n = length ss' - 1

-- | Solve - part 1 (1870)
solve :: Input -> Int
solve = count . parse

-- | Solve - part 2
solve2 :: Input -> ()
solve2 = const ()
