{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Map   as M (lookup)
import           Data.Maybe (fromJust)
import           Prelude    hiding (round)
import           Solve      (Input, Monkey (..), Operation (..), Throw (..),
                             parse, round, score, solve, solve2, turn)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

testMonkeyData :: Input
testMonkeyData = concat
             [ "Monkey 0:\n"
             , "  Starting items: 79, 98\n"
             , "  Operation: new = old * 19\n"
             , "  Test: divisible by 23\n"
             , "    If true: throw to monkey 2\n"
             , "    If false: throw to monkey 3\n"
             ]

testMonkey0 :: Monkey
testMonkey0 = Monkey [79,98] 0 (Throw (Prod 19) 23 2 3)

testTurn0 :: Monkey
testTurn0 = Monkey [] 2 (Throw (Prod 19) 23 2 3)

main :: IO ()
main = hspec $ do
  describe "Day11" $ do
    context "read monkey and list of items" $ do
      it "expect Monkey 0" $ do
        let
          ms = parse testMonkeyData
        M.lookup 0 ms `shouldBe` Just testMonkey0
    context "monkey 0 turn" $ do
      it "expect Monkeys updated and 0 to be empty" $ do
        contents <- readFile "test.data"
        let
          ms = parse contents
          w0 = fromJust (M.lookup 0 ms)
          ms' = turn 0 w0 ms
        fromJust (M.lookup 0 ms') `shouldBe` testTurn0
    context "score after 20 rounds" $ do
      it "expect score 10605" $ do
        contents <- readFile "test.data"
        score (round 20 (parse contents)) `shouldBe` 10605
    context "part 1" $
      it "expect 10605" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 10605
    context "part 2" $
      it "expect unit" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` ()
