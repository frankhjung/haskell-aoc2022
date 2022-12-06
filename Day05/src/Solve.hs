{-# LANGUAGE OverloadedStrings #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day05
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( every
             , Move (..)
             , apply
             , apply2
             , mkStack
             , parseMoves
             , parse
             , solve
             , solve2
             , Stacks
             ) where

import           Control.Monad        (void)
import           Data.Array           (Array, elems, listArray, (!), (//))
import           Data.Attoparsec.Text (Parser, decimal, endOfInput, endOfLine,
                                       many', parseOnly, space, string)
import           Data.Char            (isSpace)
import           Data.Either          (fromRight)
import           Data.List            (foldl')
import           Data.List.Split      (splitWhen)
import           Data.Text            (Text, pack)


type Stacks = Array Int (Int, String)
type Input = String

-- | Definition of a move, how many moves and from where to where.
data Move = Move {
              _count :: Int     -- | number of crates to move
            , _from  :: Int     -- | index of stack to move from
            , _to    :: Int     -- | index of stack to move to
            } deriving (Show, Eq)

-- | == Stacks

-- | Parse file input.
fileParser :: Parser [Move]
fileParser = many' (moveParser <* endOfLine)

-- | Collect every nth element from a list.
every :: Int -> [a] -> [a]
every n = map snd . filter ((== 1) . fst) . zip (cycle [1..n])

-- | Build stacks of crates from rows of crates from top to bottom.
mkStack :: Int          -- | number of stacks
           -> [String]  -- | rows of crates by stack
           -> Stacks    -- | indexed stacks as array
mkStack n ss = listArray (1,n) $ zip [1..] (mkStack' ss)
  where
    mkStack' xs
      | all null xs = []
      | otherwise = getStack xs : mkStack' (dropStack xs)
      where
        getStack = concatMap (filter (not . isSpace) . take 1)
        dropStack = map (drop 1)

-- | == Moves

-- | Parse moves.
-- @
-- move 1 from 2 to 1
-- @
moveParser :: Parser Move
moveParser = do
  void $ string "move"
  void space
  count <- decimal
  void space
  void $ string "from"
  void space
  from <- decimal
  void space
  void $ string "to"
  void space
  Move count from <$> decimal

-- | Parse input.
parseMoves :: Text -> Either String [Move]
parseMoves = parseOnly (fileParser <* endOfInput)

-- | Move some crates in an array of stacks.
--
-- Example: move 1 from 2 to 1
--
-- @
-- array (1,3) [(1,(1,"NZ")),(2,(2,"DCM")),(3,(3,"P"))]
-- λ> snd $ s!2
-- "DCM"
-- λ> snd $ s!1
-- "NZ"
-- -- so moving 1 from 2 to 1 looks like:
-- λ> foldr (:) "NZ" (reverse (take 1 "DCM"))
-- "DNZ"
-- -- now replace existing array with updated entries
-- λ> s' = s // [(1,(1,"CM")),(2,(2,"DNZ"))]
-- array (1,3) [(1,(1,"CM")),(2,(2,"DNZ")),(3,(3,"P"))]
-- @
apply :: Stacks -> Move -> Stacks
apply s m = s // [(fi,(fi,from)),(ti,(ti,to))]
  where
    fi = _from m
    ti = _to m
    f = snd (s ! fi)
    t = snd (s ! ti)
    c = _count m
    to = foldr (:) t (reverse (take c f))
    from = drop c f

-- | == Parse

-- | Read input into a stack of crates and a list of moves.
-- Parse creates by processing rows of strings and transposing to stacks.
-- Parse moves using attoparsec into a list of moves.
parse :: String -> (Stacks, [Move])
parse contents =
  let
    -- split crates & moves
    [crates, moves'] = splitWhen (=="") (lines contents)
    -- get number of crate stacks
    size = (read . last . words . last) crates :: Int
    -- extract crates from each input rows (from top to bottom)
    rows = map (every 4 . drop 1) (take size crates)
    -- build stacks of crates from rows of crates by stack
    stacks = mkStack size rows
    -- parse move instructions
    moves = fromRight [] (parseMoves (pack (unlines moves')))
  in
    (stacks, moves)

-- | Solve - part 1
solve :: Input -> String
solve content = head . snd <$> elems stacks'
  where
    (stacks, moves) = parse content
    -- apply moves to crates in stacks
    stacks' = foldl' apply stacks moves

-- | == Part 2

-- Lift and shift partial stack of crates rather than one by one.
apply2 :: Stacks -> Move -> Stacks
apply2 s m = s // [(fi,(fi,from)),(ti,(ti,to))]
  where
    fi = _from m
    ti = _to m
    f = snd (s ! fi)
    t = snd (s ! ti)
    c = _count m
    to = foldr (:) t (take c f)
    from = drop c f

-- | Solve - part 2
solve2 :: Input -> String
solve2 content = head . snd <$> elems stacks'
  where
    (stacks, moves) = parse content
    -- apply moves to crates in stacks
    stacks' = foldl' apply2 stacks moves
