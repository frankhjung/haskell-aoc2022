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
             , parseInput
             , parse
             , startState
             , run
             , runInstruction
             , getCounts
             , count
             , solve
             , setSprite
             , getSprites
             , solve2
             , Instruction (..)
             , Dump
             , Cycle
             , Register
             , Sprite
             , InstructionState
             ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Control.Monad.State  (State, evalState, get, put)
import           Data.Attoparsec.Text (Parser, decimal, endOfInput, endOfLine,
                                       isEndOfLine, many', parseOnly, signed,
                                       skipSpace, skipWhile, string, try)
import           Data.Either          (fromRight)
import           Data.Text            (Text, pack)

-- | Our input data type.
type Input = String

-- | A program consists of these two instructions.
data Instruction = Noop | Addx Int deriving (Show)

-- | == Parsers

noopParser :: Parser Instruction
noopParser = do
  void $ string "noop"
  skipWhile (not . isEndOfLine)
  return Noop

addxParser :: Parser Instruction
addxParser = do
  void $ string "addx"
  skipSpace
  x <- signed decimal
  skipWhile (not . isEndOfLine)
  return (Addx x)

-- | Parse moves from input.
lineParser :: Parser Instruction
lineParser = (try noopParser <|> addxParser) <* endOfLine

parseInput :: Text -> Either String [Instruction]
parseInput = parseOnly (many' lineParser <* endOfInput)

parse :: Input -> [Instruction]
parse = fromRight [] . parseInput . pack

-- | == State

type Cycle = Int
type Register = Int
type Sprite = Char

--- | Current cycle and register value.
type Current = (Cycle, Register, Sprite)

-- | Instruction dump is history of register by cycle.
type Dump = [(Cycle, Register, Sprite)]

-- | Instruction state.
type InstructionState = ( Current -- ^ state for current instruction
                        , Dump    -- ^ program dump
                        )

-- | Initial state.
startState :: InstructionState
startState = ((0,1,'#'), [])

-- | Run program recording outcome for each cycle.
run :: [Instruction] -> Dump
run is = evalState (runInstruction is) startState

-- | Set sprite as lit pixel or a space (as easier to read than a period).
setSprite :: Cycle -> Register -> Sprite
setSprite c r = if ((c-1) `mod` 40) `elem` [r-1,r,r+1] then '#' else ' '

-- | Recursively run instructions until end of input.
runInstruction :: [Instruction] -> State InstructionState Dump
runInstruction [] = do        -- finished running the program
  (_, dump) <- get
  return dump                 -- update program dump

runInstruction (i:is) = do
  ((c,r,_), dump) <- get
  let
    c' = c+1                  -- increment cycle
    s = setSprite c' r        -- determine sprite character for first cycle
  case i of
    Noop   -> put ((c',r,s), (c',r,s):dump)
    Addx x -> let
                dump' = (c',r,s):dump     -- first cycle of addx instruction
                s' = setSprite (c'+1) r   -- set sprite for second cycle
                r' = r+x                  -- update register at end of second cycle
              in
                put ((c'+1,r',s'), (c'+1,r,s'):dump') -- update next cycle
  runInstruction is

-- | Extract cycle and register values from dump.
getCounts :: Dump -> [(Cycle, Register)]
getCounts []           = []
getCounts ((c,r,_):ds) = (c,r):getCounts ds

clockCycles :: [Cycle]
clockCycles = [20,60,100,140,180,220]

-- | Count results for solution.
count :: Dump -> Int
count = sum . map (uncurry (*)) . filter ((`elem` clockCycles) . fst) . getCounts

-- | Solve - part 1 (13520)
solve :: Input -> Int
solve = count . run . parse

-- | == Part 2

-- | Extract sprites from dump.
getSprites :: Dump -> [Sprite]
getSprites []           = []
getSprites ((_,_,s):ds) = s:getSprites ds

-- | Solve - part 2 (PGPHBEAB)
solve2 :: Input -> [Sprite]
solve2 = reverse . getSprites . run . parse
