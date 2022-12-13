module PloySpec (spec) where

import Data.Function ((&))

import Test.Hspec
import Control.Exception.Base

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

lanceNextToEachOtherBoard :: Board
lanceNextToEachOtherBoard = ([Piece White 170, Piece Black 170, Piece White 16, Piece Black 16]++(replicate 5 Empty)) : -- Commanders and shields so that game is not finished
                            ([Piece White 84, Empty, Piece White 84]++(replicate 6 Empty)) : -- two 2 lances one cell away from each other
                            (replicate 7 emptyRow)

spec :: Spec
spec = do
  -- #################### gameFinished #########################
  describe "Module Ploy: gameFinished ..." $ do
    it "unfinished board" $ do
        gameFinished unfinishedBoard `shouldBe` (False :: Bool) 

    it "no white/black commander" $ do
        gameFinished noWhiteCommanderBoard `shouldBe` (True :: Bool) 
        gameFinished noBlackCommanderBoard `shouldBe` (True :: Bool) 

    it "only white/black commander" $ do
        gameFinished onlyWhiteCommanderBoard `shouldBe` (True :: Bool) 
        gameFinished onlyBlackCommanderBoard `shouldBe` (True :: Bool) 

  -- #################### isValidMove #########################
  describe "Module Ploy: isValidMove ..." $ do
    it "Rotate more than 7 times is not allowed" $
        evaluate (isValidMove unfinishedBoard (Move {start=Pos 'd' 7, target=Pos 'd' 6, turn=10})) `shouldThrow` errorCall "Rotate more than 7 times is not allowed"

    it "Path between start and target is not empty" $ do
        isValidMove lanceNextToEachOtherBoard (Move {start=Pos 'a' 8, target=Pos 'd' 8, turn=0}) `shouldBe` (False :: Bool)
    
    it "Cell at target position is occupied by a piece of the same color" $ do
        isValidMove lanceNextToEachOtherBoard (Move {start=Pos 'a' 8, target=Pos 'c' 8, turn=0}) `shouldBe` (False :: Bool)

    it "Invalid direction" $ do
        isValidMove unfinishedBoard (Move {start=Pos 'd' 7, target=Pos 'c' 6, turn=0}) `shouldBe` (False :: Bool)
        isValidMove unfinishedBoard (Move {start=Pos 'c' 8, target=Pos 'b' 7, turn=0}) `shouldBe` (False :: Bool)
        isValidMove unfinishedBoard (Move {start=Pos 'b' 9, target=Pos 'a' 8, turn=0}) `shouldBe` (False :: Bool)

    context "Invalid number of movements and/or turns" $ do
        it "Shield" $ do
            isValidMove unfinishedBoard (Move {start=Pos 'd' 7, target=Pos 'd' 5, turn=0}) `shouldBe` (False :: Bool)
        it "Probe" $ do
            isValidMove unfinishedBoard (Move {start=Pos 'c' 8, target=Pos 'c' 5, turn=0}) `shouldBe` (False :: Bool)
            isValidMove unfinishedBoard (Move {start=Pos 'c' 8, target=Pos 'c' 6, turn=1}) `shouldBe` (False :: Bool)
        it "Lance" $ do
            isValidMove unfinishedBoard (Move {start=Pos 'b' 9, target=Pos 'b' 5, turn=0}) `shouldBe` (False :: Bool)
            isValidMove unfinishedBoard (Move {start=Pos 'b' 9, target=Pos 'b' 6, turn=1}) `shouldBe` (False :: Bool)
        it "Commander" $ do
            isValidMove lanceNextToEachOtherBoard (Move {start=Pos 'a' 9, target=Pos 'c' 7, turn=0}) `shouldBe` (False :: Bool)
            isValidMove lanceNextToEachOtherBoard (Move {start=Pos 'a' 9, target=Pos 'b' 8, turn=1}) `shouldBe` (False :: Bool)

    it "Valid movement of different pieces" $ do
        isValidMove unfinishedBoard (Move {start=Pos 'd' 7, target=Pos 'd' 6, turn=0}) `shouldBe` (True :: Bool)
        isValidMove unfinishedBoard (Move {start=Pos 'c' 8, target=Pos 'c' 7, turn=0}) `shouldBe` (True :: Bool)
        isValidMove unfinishedBoard (Move {start=Pos 'c' 9, target=Pos 'b' 8, turn=0}) `shouldBe` (True :: Bool)
