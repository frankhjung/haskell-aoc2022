module Main (main) where

import           Solve      (solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Day06" $ do
    context "part 1" $ do
      it "expect 5" $
        solve "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      it "expect 6" $
        solve "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
      it "expect 7" $
        solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 7
      it "expect 10" $
        solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      it "expect 11" $ do
        solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11
    context "part 2" $ do
      it "expect 19" $
        solve2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19
      it "expect 23" $
        solve2 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23
      it "expect 23" $
        solve2 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 23
      it "expect 26" $
        solve2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 26
      it "expect 29" $
        solve2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 29

