{-# LANGUAGE OverloadedStrings #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day 11
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( Id
             , Input
             , Monkey (..)
             , MonkeyMap
             , MonkeyTuple
             , Operation (..)
             , Throw (..)
             , ThrowMap
             , WorryLevel
             , addWorry
             , doOp
             , worry
             , worry2
             , getMonkeyId
             , idParser
             , itemsParser
             , monkeyParser
             , opParser
             , parse
             , parseInput
             , round
             , round2
             , score
             , solve
             , solve2
             , throw
             , throw2
             , turn
             , turn2
             ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Attoparsec.Text (Parser, char, decimal, endOfInput,
                                       endOfLine, parseOnly, sepBy, sepBy1,
                                       skipSpace, string)
import           Data.Either          (fromRight)
import           Data.Ix              (range)
import           Data.List            (foldl', sortOn)
import           Data.Map             (Map)
import           Data.Map             as M (adjust, elems, foldr, fromList,
                                            insert, size, (!))
import           Data.Ord             (Down (..))
import           Data.Sequence        (Seq (..))
import           Data.Sequence        as S (fromList, (|>))
import           Data.Text            (Text, pack)
import           Prelude              hiding (round)

-- | Our input data type.
type Input = String

-- | Monkey identifier (key used by map).
type Id = Int

-- | Human worry level.
type WorryLevel = Int

-- | Parse input into Monkey and Throw by Monkey Id.
type MonkeyTuple = (Id, Monkey, Throw)

-- | Monkey is collection of item worries, inspected items and throw rules.
data Monkey = Monkey {
                _levels :: Seq WorryLevel -- ^ items currently held
              , _throws :: Int            -- ^ count of items throw
              } deriving (Show, Eq)

-- | A `Throw` consists of a divisibility function and a new monkey id
-- based on test outcome.
data Throw = Throw {
                _op    :: Operation -- ^ pre-test operation
              , _div   :: Int       -- ^ modulus test
              , _true  :: Int       -- ^ monkey to throw to if divisible
              , _false :: Int       -- ^ monkey to throw to not divisible
             } deriving (Show, Eq)

-- | Pre-inspection `Operation`.
data Operation = Sum Int | Prod Int | Square deriving (Show, Eq)

-- | Store items held by each monkey.
type MonkeyMap = Map Id Monkey

-- | Store throw rules.
type ThrowMap = Map Id Throw

-- | == Parsers

monkeyParser :: Parser MonkeyTuple
monkeyParser = do
  i <- idParser       -- monkey id
  w <- itemsParser    -- items by worry value
  o <- opParser       -- operation
  p <- throwDiv       -- throw test
  t <- throwTrue      -- monkey to throw to if true
  f <- throwFalse     -- monkey to throw to if false
  return (i, Monkey w 0, Throw o p t f)

idParser :: Parser Int
idParser = do
  void $ string "Monkey "
  i <- decimal
  void $ string ":"
  endOfLine
  return i

itemsParser :: Parser (Seq Int)
itemsParser = do
  void $ string "  Starting items: "
  ws <- decimal `sepBy1` string ", "
  endOfLine
  return (S.fromList ws)

opParser :: Parser Operation
opParser = do
  void $ string "  Operation: new = old "
  o <- sumOp <|> prodOp <|> squareOp
  endOfLine
  return o

sumOp :: Parser Operation
sumOp = do
  void $ char '+'
  skipSpace
  Sum <$> decimal

prodOp :: Parser Operation
prodOp = do
  void $ char '*'
  skipSpace
  Prod <$> decimal

squareOp :: Parser Operation
squareOp = do
  void $ string "* old"
  return Square

throwDiv :: Parser Int
throwDiv = do
  void $ string "  Test: divisible by "
  n <- decimal        -- divisibility value to test for
  endOfLine
  return n

throwTrue :: Parser Int
throwTrue = do
  void $ string "    If true: throw to monkey "
  t <- decimal        -- if true throw to this monkey
  endOfLine
  return t

throwFalse :: Parser Int
throwFalse = do
  void $ string "    If false: throw to monkey "
  f <- decimal        -- if false throw to this monkey
  endOfLine
  return f

parseInput :: Text -> Either String [MonkeyTuple]
parseInput = parseOnly ((monkeyParser `sepBy` endOfLine) <* endOfInput)

parse :: Input -> (MonkeyMap, ThrowMap)
parse = go . fromRight [] . parseInput . pack
  where
    go :: [(Id, Monkey, Throw)] -> (MonkeyMap, ThrowMap)
    go xs = (M.fromList ms, M.fromList ts)
      where
        ms = map (\(i,m,_) -> (i,m)) xs -- split monkeys
        ts = map (\(i,_,t) -> (i,t)) xs -- split throw rules

-- | == Monkey Round
-- A round consists of each monkey in turn throwing items.

-- | Score:
-- Get score of the two monkey's that have thrown the most.
score :: MonkeyMap -> Integer
score = product . map toInteger . take 2 . sortOn Down . levels
  where
    levels :: MonkeyMap -> [Int]
    levels ls = map _throws (elems ls)

-- | Round:
-- A round performs n throws.
--
-- TODO
--
-- Replace ThrowMap -> MonkeyMap with tuple (Adjustment, ThrowMap, MonkeyMap)
-- Where Adjustment is a partial function: @(Int -> Int) -> Int@
-- Part 1 = @flip div 3@
-- Part 2 = @flip rem (product of divisors)@
-- This should reduce the code ... (or Monad Transformer, State Monad?)
round :: Int -> (MonkeyMap, ThrowMap) -> MonkeyMap
round n (m,t) = last $ take (succ n) $ iterate (turn t) m

-- | Turn:
-- A throw lets all monkeys have a turn at throwing their stash of items.
turn :: ThrowMap -> MonkeyMap -> MonkeyMap
turn ts ms = foldl' turn' ms (range (0, M.size ms - 1))
  where
    turn' :: MonkeyMap -> Id -> MonkeyMap
    turn' mm i = throw ts i (mm ! i) mm

-- | Throw:
-- Monkey's turn to throw all it's items.
--
-- Monkey i has no more items to throw, so save monkeys state
throw :: ThrowMap -> Id -> Monkey -> MonkeyMap -> MonkeyMap
throw _ i (Monkey Empty s) ms = M.insert i (Monkey Empty s) ms

-- Monkey has more items to throw ...
throw ts i (Monkey (w:<|ws) s) ms =
  let
    s' = s + 1                              -- one more item thrown
    ms' = worry ts i w ms                   -- update worry level
  in throw ts i (Monkey ws s') ms'          -- process next worry level

-- | worry:
-- Update worry level
--
-- Id is money whose throw rules we use.
-- WorryLevel is current value to inspect.
-- A updated monkey map is returned with monkey been thrown to.
-- Part 1 - take 1/3 of worry level
worry :: ThrowMap -> Id -> WorryLevel -> MonkeyMap -> MonkeyMap
worry ts i w = M.adjust (addWorry w') i'    -- update monkey catching item
  where
    w' = doOp (_op (ts!i)) w `div` 3        -- updated worry level
    i' = getMonkeyId w' (ts!i)              -- index to monkey been thrown to

-- | Add worry level to end of monkey.
addWorry :: WorryLevel -> Monkey -> Monkey
addWorry l (Monkey w s) = Monkey (w |> l) s

-- | Perform worry operation to give a new worry level.
doOp :: Operation -> WorryLevel -> WorryLevel
doOp (Sum s) w  = s + w
doOp (Prod p) w = p * w
doOp Square w   = w * w

-- | Monkey to next throw to ...
getMonkeyId :: WorryLevel -> Throw -> Id
getMonkeyId w t = if w `mod` _div t == 0 then _true t else _false t

-- | Solve - part 1 (58794)
solve :: Input -> Integer
solve = score . round 20 . parse

-- | == Part 2

-- | Round2:
-- A round performs n throws.
round2 :: Int -> (MonkeyMap, ThrowMap) -> MonkeyMap
round2 n (m,t) = last $ take (succ n) $ iterate (turn2 t) m

-- | Turn2:
-- A throw lets all monkeys have a turn at throwing their stash of items.
turn2 :: ThrowMap -> MonkeyMap -> MonkeyMap
turn2 ts ms = foldl' turn' ms (range (0, M.size ms - 1))
  where
    turn' :: MonkeyMap -> Id -> MonkeyMap
    turn' mm i = throw2 ts i (mm ! i) mm

-- | Throw2:
-- Monkey's turn to throw all it's items.
--
-- Monkey i has no more items to throw, so save monkeys state
throw2 :: ThrowMap -> Id -> Monkey -> MonkeyMap -> MonkeyMap
throw2 _ i (Monkey Empty s) ms = M.insert i (Monkey Empty s) ms

-- Monkey has more items to throw ...
throw2 ts i (Monkey (w:<|ws) s) ms =
  let
    s' = s + 1                              -- one more item thrown
    ms' = worry2 ts i w ms                  -- update worry level
  in throw2 ts i (Monkey ws s') ms'         -- process next worry level

-- | worry2:
-- Update worry level - part 2
--
-- - Id is monkey whose throw rules we use.
-- - WorryLevel is from the current item being inspected.
-- - A updated map is returned with monkey been thrown to.
worry2 :: ThrowMap -> Id -> WorryLevel -> MonkeyMap -> MonkeyMap
worry2 ts i w = M.adjust (addWorry w'') i'  -- update monkey catching item
  where
    o' = _op (ts!i)                         -- opertion for this monkey
    w' = doOp o' w                          -- updated worry level
    i' = getMonkeyId w' (ts!i)              -- index to monkey been thrown to
    w'' = rem w' (worryAdjust ts)           -- updated worry level
    worryAdjust :: ThrowMap -> Id           -- worry adjustment value.
    worryAdjust = M.foldr ((*) . _div) 1    -- product of all divisors

-- | Solve - part 2 (20151213744)
solve2 :: Input -> Integer
solve2 = score . round2 10000 . parse
