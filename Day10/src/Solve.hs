{-# LANGUAGE OverloadedStrings #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day10
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( addxParser
             , noopParser
             , lineParser
             , parseContents
             , parse
             , startState
             , run
             , runInstruction
             , solve
             , solve2
             , Program (..)
             , Dump
             , Cycle
             , Register
             , ProgramState
             ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Control.Monad.State  (State, evalState, get, put)
import           Data.Attoparsec.Text (Parser, decimal, endOfInput, endOfLine,
                                       isEndOfLine, many', parseOnly, signed,
                                       skipSpace, skipWhile, string, try)
import           Data.Either          (fromRight)
import           Data.Text            (Text, pack)

type Input = String

-- | A program consists of these two instructions.
data Program = Noop | Addx Int deriving (Show)

-- | == Parsers

noopParser :: Parser Program
noopParser = do
  void $ string "noop"
  skipWhile (not . isEndOfLine)
  return Noop

addxParser :: Parser Program
addxParser = do
  void $ string "addx"
  skipSpace
  x <- signed decimal
  skipWhile (not . isEndOfLine)
  return (Addx x)

-- | Parse moves from input.
lineParser :: Parser Program
lineParser = (try noopParser <|> addxParser) <* endOfLine

parseContents :: Text -> Either String [Program]
parseContents = parseOnly (many' lineParser <* endOfInput)

parse :: String -> [Program]
parse = fromRight [] . parseContents . pack

-- | == State

type Cycle = Int
type Register = Int

--- | Current working directory, and accumulated size.
-- Cycle
type Current = (Cycle, Register)

-- | Program dump is history of register by cycle.
type Dump = [(Cycle, Register)]

-- | Program state.
type ProgramState = ( Current -- ^ state for current instruction
                    , Dump    -- ^ program dump
                    )

-- | Initial state.
startState :: ProgramState
startState = ((0,1), [])

-- | Run program recording outcome for each cycle.
-- run :: [Program] -> [Register]
run :: [Program] -> Dump
run is = evalState (runInstruction is) startState

-- | Run instruction.
runInstruction :: [Program] -> State ProgramState Dump
runInstruction [] = do        -- finished running the program
  (_, dump) <- get
  return dump                 -- update program dump

runInstruction (i:is) = do
  ((c,r), dump) <- get
  case i of
    Noop   -> put ((succ c,r), (succ c,r):dump)
    Addx x -> let
                c' = succ c
                dump' = (c',r):dump   -- first cycle of addx instruction
                r' = r+x              -- second cycle of addx, update register
              in
                put ((succ c',r'), (succ c',r):dump')
  runInstruction is

-- | Count results for solution
count :: Dump -> Int
count = sum . map (uncurry (*)) . filter ((`elem` [20,60,100,140,180,220]) . fst)

-- | Solve - part 1
solve :: Input -> Int
solve = count . run . parse

-- | Solve - part 2
solve2 :: Input -> ()
solve2 = const ()
