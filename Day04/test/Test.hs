{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Text             (Text)
import           Solve                 (contains, count, rangeParser,
                                        recordParser, solve)
import           Test.Hspec            (context, describe, hspec, it, shouldBe)
import           Test.Hspec.Attoparsec (shouldFailOn, shouldSucceedOn)

main :: IO ()
main = hspec $ do
  describe "Day04" $ do
    context "range parser" $ do
      it "2-5 parses as Range (2,5)" $
        rangeParser `shouldSucceedOn` ("2-5,3-4\n" :: Text)
      it "2,5 fails" $
        rangeParser `shouldFailOn` ("2,5\n" :: Text)
    context "record parser" $ do
      it "2-5,3-4 parses as Record (2,5) (3,4)" $
        recordParser `shouldSucceedOn` ("2-5,3-4\n" :: Text)
      it "2-5-3-4 fails" $
        recordParser `shouldFailOn` ("2-5-3-4" :: Text)
    context "contains" $ do
      it "(2,5) contains (3,4)" $
        (2,5) `contains` (3,4)
      it "(3,4) is contained in (2,5)" $
        (3,4) `contains` (2,5)
      it "(2,8) contains (3,7)" $
        (2,8) `contains` (3,7)
      it "(6,6) is contained in (4,6)" $
        (6,6) `contains` (4,6)
    context "not contains" $ do
      it "(2,5) does not contain (5,6)" $
        (2,5) `contains` (5,6) `shouldBe` False
      it "(5,6) does not contain (2,5)" $
        (5,6) `contains` (2,5) `shouldBe` False
      it "(2,4) does not contain (6,8)" $
        (2,4) `contains` (6,8) `shouldBe` False
    context "count" $ do
      it "[True,True,False] is 2" $
        count [True,True,False] `shouldBe` 2
      it "[False,False,False] is 0" $
        count [False,False,False] `shouldBe` 0
    context "part 1" $
      it "expected 2" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 2
