module Main (main) where

import           Data.Array (elems)
import           Solve      (Move (..), parse, solve)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do

  describe "Day05" $ do

    context "parse stack input" $
      it "expect stacks NZ,DCM,P" $ do
        contents <- readFile "test.data"
        let (stacks, _) = parse contents
        elems stacks `shouldBe` [(1,"NZ"),(2,"DCM"),(3,"P")]

    context "parse move input" $
      it "expect Move 1 2 1" $ do
        contents <- readFile "test.data"
        let (_, moves) = parse contents
            firstMove = Move 1 2 1
        head moves `shouldBe` firstMove

    context "part 1" $
      it "expect crates CMZ" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` "CMZ"
