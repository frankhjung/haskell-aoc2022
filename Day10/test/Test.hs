module Main (main) where

import           Solve      (Sprite, solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

-- Expected output from test data.
-- I've replace '.' with a space is easier to read the end result.
part2 :: [Sprite]
part2 = concat [ "##  ##  ##  ##  ##  ##  ##  ##  ##  ##  "
               , "###   ###   ###   ###   ###   ###   ### "
               , "####    ####    ####    ####    ####    "
               , "#####     #####     #####     #####     "
               , "######      ######      ######      ####"
               , "#######       #######       #######     "
               ]

main :: IO ()
main = hspec $ do
  describe "Day10" $ do
    context "part 1" $ do
      it "expect 13140" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 13140
    context "part 2" $
      it "expect sprite mosaic" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` part2
