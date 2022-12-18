{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Map      as M ((!))
import           Data.Sequence (Seq (..))
import           Data.Sequence as S (fromList)
import           Solve         (Input, Monkey (..), Operation (..), Throw (..),
                                parse, solve, solve2, throw)
import           Test.Hspec    (context, describe, hspec, it, shouldBe)

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
testMonkey0 = Monkey (S.fromList [79,98]) 0

testThrow0 :: Throw
testThrow0 = Throw (Prod 19) 23 2 3

testTurn0 :: Monkey
testTurn0 = Monkey Empty 2

main :: IO ()
main = hspec $ do
  describe "Day11" $ do
    context "read monkey" $ do
      it "expect Monkey 0" $ do
        let
          (ms,_) = parse testMonkeyData
        ms!0 `shouldBe` testMonkey0
    context "read throw" $ do
      it "expect Throw 0" $ do
        let
          (_,ts) = parse testMonkeyData
        ts!0 `shouldBe` testThrow0
    context "monkey 0 turn" $ do
      it "expect Monkeys updated and 0 to be empty" $ do
        contents <- readFile "test.data"
        let
          (ms,ts) = parse contents
          w0 = ms!0
          ms' = throw ts 0 w0 ms
        ms'!0 `shouldBe` testTurn0
    context "part 1" $
      it "expect 10605" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 10605
    context "part 2" $
      it "expect 2713310158" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` 2713310158
