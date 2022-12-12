module PloySpec (spec) where

import Data.Function ((&))

import Test.Hspec

import Board 
import Ploy

whitePart, blackPart :: Board
whitePart = [[Piece White 170,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty]]
blackPart = [[Piece Black 170,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Empty,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

emptyRow :: [Cell]
emptyRow = Empty & replicate 9

unfinishedBoard :: Board
unfinishedBoard = whitePart ++ (replicate 3 emptyRow) ++ blackPart

noWhiteCommanderBoard :: Board
noWhiteCommanderBoard = ((Empty : (tail (head whitePart))) : tail whitePart) ++ (replicate 3 emptyRow) ++ blackPart

noBlackCommanderBoard :: Board
noBlackCommanderBoard = whitePart ++ (replicate 3 emptyRow) ++ ((Empty : (tail (head blackPart))) : tail blackPart)

onlyWhiteCommanderBoard :: Board
onlyWhiteCommanderBoard = [Piece White 170 : (replicate 8 Empty)] ++ (replicate 5 emptyRow) ++ blackPart

onlyBlackCommanderBoard :: Board
onlyBlackCommanderBoard = whitePart ++ (replicate 5 emptyRow) ++ [Piece Black 170 : (replicate 8 Empty)]

spec :: Spec
spec = do
  describe "Module Ploy: gameFinished ..." $ do
    it "unfinished board" $ do
        gameFinished unfinishedBoard `shouldBe` (False :: Bool) 

    it "no white commander" $ do
        gameFinished noWhiteCommanderBoard `shouldBe` (True :: Bool) 

    it "no black commander" $ do
        gameFinished noBlackCommanderBoard `shouldBe` (True :: Bool) 

    it "only white commander" $ do
        gameFinished onlyWhiteCommanderBoard `shouldBe` (True :: Bool) 

    it "only black commander" $ do
        gameFinished onlyBlackCommanderBoard `shouldBe` (True :: Bool) 
