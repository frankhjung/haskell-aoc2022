{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Map   as M (lookup)
import           Data.Text  (Text)
import           Solve      (Command (..), build, parse, solve, solve2)
import           Test.Hspec (context, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do

  describe "parse input lines" $ do
    context "parse root dir" $
      it "expect ChangeDir /" $ do
        let commands = parse "$ cd /\n"
        head commands `shouldBe` ChangeDir ("/" :: Text)
    context "parse back dir" $
      it "expect ChangeDir .." $ do
        let commands = parse "$ cd ..\n"
        head commands `shouldBe` ChangeDir (".." :: Text)
    context "parse cd" $
      it "expect ChangeDir abc" $ do
        let commands = parse "$ cd abc\n"
        head commands `shouldBe` ChangeDir ("abc" :: Text)
    context "parse dir" $
      it "expect Dir abc" $ do
        let commands = parse "dir abc\n"
        head commands `shouldBe` Dir ("abc" :: Text)
    context "parse ls" $
      it "expect LS" $ do
        let commands = parse "$ ls\n"
        head commands `shouldBe` ListDir
    context "parse file" $
      it "expect File abc.xyz 123" $ do
        let commands = parse "123 abc.xyz\n"
        head commands `shouldBe` File "abc.xyz" 123

  describe "build directory map" $ do
    context "root dir /" $
      it "expect / = 48381165" $ do
        contents <- readFile "test.data"
        let dm = build $ parse contents
        M.lookup ["/"] dm `shouldBe` Just 48381165
    context "/a" $
      it "expect /a = 94853" $ do
        contents <- readFile "test.data"
        let dm = build $ parse contents
        M.lookup ["a","/"] dm `shouldBe` Just 94853
    context "/d" $
      it "expect /d = 24933642" $ do
        contents <- readFile "test.data"
        let dm = build $ parse contents
        M.lookup ["d","/"] dm `shouldBe` Just 24933642
    context "/a e" $
      it "expect /a e = 584" $ do
        contents <- readFile "test.data"
        let dm = build $ parse contents
        M.lookup ["e", "a","/"] dm `shouldBe` Just 584

  describe "solve" $ do
    context "part 1" $
      it "expect 95437" $ do
        contents <- readFile "test.data"
        solve contents `shouldBe` 95437
    context "part 2" $
      it "expect unit" $ do
        contents <- readFile "test.data"
        solve2 contents `shouldBe` ()
