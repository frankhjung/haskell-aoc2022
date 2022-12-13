{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-|

Module      : Solve
Description : Advent of Code 2022 Day07
Copyright   : © Frank Jung, 2022
License     : GPL-3

-}

module Solve ( count
             , count2
             , cdParser
             , dirParser
             , fileParser
             , lineParser
             , lsParser
             , parse
             , parseContents
             , solve
             , solve2
             , startState
             , build
             , buildDir
             , addDir
             , Command (..)
             , DirWork
             , DirMap
             , DirState
             ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (void)
import           Control.Monad.State  (State, evalState, get, put)
import           Data.Attoparsec.Text (Parser, decimal, endOfInput, endOfLine,
                                       isEndOfLine, many', parseOnly, space,
                                       string, takeTill, try)
import           Data.Either          (fromRight)
import           Data.List            (find, sort, tails)
import           Data.Map             (Map)
import           Data.Map             as M (elems, empty, fromList, unionsWith)
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text, pack)

type Input = String

-- | Record commands from input.
data Command = ChangeDir Text | ListDir | Dir Text | File Text Integer
                deriving (Show, Eq)

-- | = Parse input commands.

--  $ cd /    - set root dir with value 0
--  $ cd ..   - lookup previous dir where dir path is [Text] & read size
--  $ cd xyz  - new directory?
--  $ ls      - nothing (will be updating current dir)
--  dir xyz   - nothing (will be CD into it later)
--  123 abc.d - add n to current dir size (ignore name)

cdParser :: Parser Command
cdParser = do
  void $ string "$"
  void space
  void $ string "cd"
  void space
  _name <- takeTill isEndOfLine
  return (ChangeDir _name)

lsParser :: Parser Command
lsParser = do
  void $ string "$"
  void space
  void $ string "ls"
  return ListDir

dirParser :: Parser Command
dirParser = do
  void $ string "dir"
  void space
  _name <- takeTill isEndOfLine
  return (Dir _name)

fileParser :: Parser Command
fileParser = do
  _size <- decimal
  void space
  _name <- takeTill isEndOfLine
  return (File _name _size)

lineParser :: Parser Command
lineParser = (try cdParser <|> lsParser <|> dirParser <|> fileParser) <* endOfLine

parseContents :: Text -> Either String [Command]
parseContents = parseOnly (many' lineParser <* endOfInput)

parse :: String -> [Command]
parse = fromRight [] . parseContents . pack

-- | = State

--- | Current working directory, and accumulated size.
type DirWork = ([Text], Integer)

-- | Map of all directories.
type DirMap = Map [Text] Integer

-- | State to capture directory structure and sizes.
type DirState = ( DirWork -- ^ current working directory
                , DirMap  -- ^ map of all directories
                )

-- | Initial state is root directory.
startState :: DirState
startState = ((["/"], 0), M.empty)

-- | = Build file system from commands.

buildDir :: [Command] -> State DirState DirMap
buildDir [] = do    -- finished processing file system structure
  (wd, md) <- get
  return (addDir wd md)

-- | Build file system, acruing file sizes into parent directory.
-- if cd /   -> root dir (first entry)
-- if cd ..  -> up dir
-- if cd x   -> x is a dir
-- otherwise -> not a dir
buildDir (c:cs) = do
  (wd@(d,s), md) <- get
  case c of
    ChangeDir "/"  -> put (wd, md)                     -- root dir
    ChangeDir ".." -> put ((tail d, 0), addDir wd md)  -- up one dir
    ChangeDir cd   -> put ((cd:d, 0), addDir wd md)    -- change into dir
    ListDir        -> put (wd, md)                     -- ignore
    Dir _          -> put (wd, md)                     -- ignore
    File _ fs      -> put ((d, s+fs), md)              -- accrue file sizes
  buildDir cs

-- | Add current directory and update parents sizes.
addDir :: DirWork -> DirMap -> DirMap
addDir (d,s) dm = M.unionsWith (+) [dm, dm']
  where
    ds = (init . tails) d             -- parent directories
    dm' = M.fromList $ fmap (, s) ds  -- initialise with size to add to each

-- | Build directory file system of sizes.
build :: [Command] -> DirMap
build cs = evalState (buildDir cs) startState

count :: DirMap -> Integer
count = sum . filter (<= 100000) . elems

-- | Solve - part 1 (1517599)
solve :: Input -> Integer
solve = count . build . parse

count2 :: DirMap -> Integer
count2 dirs =
  let
    sizes = sort $ elems dirs
    spare = 70000000 - last sizes
    best = find ((>= 30000000) . (spare +)) sizes
  in
    fromMaybe 0 best

-- | Solve - part 2
solve2 :: Input -> Integer
solve2 = count2 . build . parse
