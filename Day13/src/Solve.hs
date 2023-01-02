-- {-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TupleSections     #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day 13 - Distress Signal
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( Packet
             , parse
             , solve
             , solve2) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Data.Attoparsec.Text (Parser, char, decimal, endOfInput,
                                       endOfLine, parseOnly, sepBy, sepBy1,
                                       skipSpace, string)

-- | Our input data type.
type Input = String

-- | A Signal is a pair of packets. A packet is an List of Int's or more
-- packets.
data Packet = IntPacket Int | ListPacket [Packet] deriving (Show, Eq)

-- | Parse input into pairs of `Packets`.
-- TODO convert pairs to list of packets?
parse :: Input -> [(Packet,Packet)]
parse = undefined

-- | Solve - part 1
solve :: Input -> Int
solve = const 0

-- | Solve - part 2
solve2 :: Input -> ()
solve2 = const ()
