{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts  #-}
-- {-# LANGUAGE TupleSections     #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day11
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( MonkeyTuple
             , MonkeyMap
             , Monkey (..)
             , Operation (..)
             , Throw (..)
             , Input
             , Id
             , WorryLevel
             , parse
             , parseInput
             , monkeyParser
             , idParser
             , itemsParser
             , opParser
             , doOp
             , getMonkeyId
             , updateItems
             , turn
             , throw
             , round
             , score
             , solve
             , solve2) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Attoparsec.Text (Parser, char, decimal, endOfInput,
                                       endOfLine, parseOnly, sepBy, sepBy1,
                                       skipSpace, string)
import           Data.Either          (fromRight)
import           Data.Ix              (range)
import           Data.List            (foldl', sortOn)
import           Data.Map             (Map)
import           Data.Map             as M (elems, fromList, insert, lookup,
                                            size)
import           Data.Maybe           (fromJust)
import           Data.Ord             (Down (..))
import           Data.Text            (Text, pack)
import           Prelude              hiding (round)

-- | Our input data type.
type Input = String

-- | Monkey identifier (key used by map).
type Id = Int

-- | Human worry level.
type WorryLevel = Int

-- | Map worry items by monkey identifier.
type MonkeyTuple = (Id, Monkey)

type MonkeyMap = Map Id Monkey

-- | Monkey is collection of item worries, inspected items and throw rules.
data Monkey = Monkey {
                _items  :: [WorryLevel] -- ^ items currently held
              , _throws :: Int          -- ^ count of items throw
              , _throw  :: Throw        -- ^ `Throw` action
              } deriving (Show, Eq)

-- | Pre-inspection `Operation`.
data Operation = Sum Int | Prod Int | Square deriving (Show, Eq)

-- | A `Throw` consists of a predicate function and an action based on the
-- outcome.
data Throw = Throw {
                _op        :: Operation -- ^ pre-test operation
              , _predicate :: Int       -- ^ modulus test
              , _true      :: Int       -- ^ monkey to throw to if divisible
              , _false     :: Int       -- ^ monkey to throw to not divisible
             } deriving (Show, Eq)

-- | == Parsers

monkeyParser :: Parser MonkeyTuple
monkeyParser = do
  i <- idParser
  is <- itemsParser
  op <- opParser
  p <- throwPredicate
  t <- throwTrue
  f <- throwFalse
  return (i, Monkey is 0 (Throw op p t f))

idParser :: Parser Int
idParser = do
  void $ string "Monkey "
  i <- decimal
  void $ string ":"
  endOfLine
  return i

itemsParser :: Parser [Int]
itemsParser = do
  void $ string "  Starting items: "
  is <- decimal `sepBy1` string ", "
  endOfLine
  return is

opParser :: Parser Operation
opParser = do
  void $ string "  Operation: new = old "
  op <- sumOp <|> prodOp <|> squareOp
  endOfLine
  return op

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

throwPredicate :: Parser Int
throwPredicate = do
  void $ string "  Test: divisible by "
  p <- decimal        -- divisibility value to test for
  endOfLine
  return p

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

parse :: Input -> MonkeyMap
parse = fromList . fromRight [] . parseInput . pack

-- | == Monkey Turn

-- | Perform worry operation to give a new worry level.
doOp :: Operation -> Int -> Int
doOp (Sum s) w  = (w + s) `div` 3
doOp (Prod p) w = (w * p) `div` 3
doOp Square w   = (w * w) `div` 3

-- | Monkey to next throw to ...
getMonkeyId :: WorryLevel -> Throw -> Id
getMonkeyId wl t = if rem wl (_predicate t) == 0 then _true t else _false t

-- | Update Monkey items.
updateItems :: Id -> Monkey -> MonkeyMap -> MonkeyMap
updateItems = M.insert

-- | Monkey turn to throw all it's items.
--
-- monkey i has no more items to throw, so save monkeys state
turn :: Id -> Monkey -> MonkeyMap -> MonkeyMap
turn i (Monkey [] s t) ms = M.insert i (Monkey [] s t) ms

--  id worry seen oper pred true false
turn i (Monkey (w:ws) s t) ms =
  let
    s' = s + 1                      -- one more item thrown
    wl = doOp (_op t) w             -- updated worry level
    i' = getMonkeyId wl t           -- index to monkey ...
    m' = fromJust (M.lookup i' ms)  -- monkey to update
    ws' = _items m' ++ [wl]         -- append worry - change to Data.Sequence
    ms' = updateItems i' (Monkey ws' (_throws m') (_throw m')) ms
  in turn i (Monkey ws s' t) ms'

-- | A throw lets all monkeys have a turn at throwing their stash of items.
throw :: MonkeyMap -> MonkeyMap
throw ms = foldl' turnMonkey ms is
  where
    is = range (0, M.size ms - 1)
    turnMonkey :: MonkeyMap -> Id -> MonkeyMap
    turnMonkey mm i = turn i (getMonkey i mm) mm
    getMonkey :: Id -> MonkeyMap -> Monkey
    getMonkey i mm = fromJust (M.lookup i mm)

-- | A round performs n throws.
round :: Int -> MonkeyMap -> MonkeyMap
round n mm = last $ take (succ n) $ iterate throw mm

-- | Score
-- Get score of the two monkey's that have thrown the most.
score :: MonkeyMap -> Int
score = product . take 2 . sortOn Down . levels
  where
    levels :: MonkeyMap -> [Int]
    levels ls = map _throws (elems ls)

-- | Solve - part 1 (58794)
solve :: Input -> Int
solve = score . round 20 . parse

-- | Solve - part 2
solve2 :: Input -> ()
solve2 = const ()
