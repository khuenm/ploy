module BoardSpec (spec) where

import Data.Function ((&))

import Test.Hspec
import Control.Exception.Base

import Board

validFEN :: String
validFEN = ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,"

validEmptyFEN :: String
validEmptyFEN = ",,,,,,,,/" & replicate 9 & concat & init

fewRowsFEN :: String
fewRowsFEN = take (9*5+8) validEmptyFEN

manyRowsFEN :: String
manyRowsFEN = validEmptyFEN ++ ",,,,,,,,/"

fewCellsFEN :: String
fewCellsFEN = drop 2 validEmptyFEN

manyCellsFEN :: String
manyCellsFEN = ",," ++ validEmptyFEN

invalidCharFEN :: String
invalidCharFEN = "h21" ++ validEmptyFEN

invalidNumFEN :: String
invalidNumFEN = "w342" ++ validEmptyFEN

validBoard :: Board
validBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

validEmptyBoard :: Board
validEmptyBoard = Empty & replicate 9 & replicate 9

spec :: Spec
spec = do
  -- #################### validateFEN #########################
  describe "Module Board: validateFEN ..." $ do
    it "FEN does not have 9 rows" $ do
        validateFEN fewRowsFEN `shouldBe` (False :: Bool)

    it "FEN has more than 9 rows" $ do
        validateFEN manyRowsFEN `shouldBe` (False :: Bool)

    it "FEN does not have 9 cells in a row" $ do
        validateFEN fewCellsFEN `shouldBe` (False :: Bool)

    it "FEN has more than 9 cells in a row" $ do
        validateFEN manyCellsFEN `shouldBe` (False :: Bool)

    it "FEN contains invalid character encoding" $ do
        validateFEN invalidCharFEN `shouldBe` (False :: Bool)

    it "FEN contains invalid number encoding" $ do
        validateFEN invalidNumFEN `shouldBe` (False :: Bool)

    it "valid FEN input" $ do
        validateFEN validFEN  `shouldBe` (True :: Bool)

  -- #################### buildBoard #########################
  describe "Module Board: buildBoard ..." $ do
    it "invalid FEN" $ 
        evaluate (buildBoard fewRowsFEN) `shouldThrow` errorCall "Input string is not in FEN notation"

    it "valid empty Board" $ do
        buildBoard validEmptyFEN `shouldBe` validEmptyBoard

    it "valid full Board" $ do
        buildBoard validFEN `shouldBe` validBoard

  -- #################### line #########################
  describe "Module Board: line ..." $ do
    it "invalid positions in column" $
        evaluate (line (Pos {col='A', row=7}) (Pos {col='b', row=7})) `shouldThrow` errorCall "Input positions are not valid"

    it "invalid positions in row" $
        evaluate (line (Pos {col='b', row=10}) (Pos {col='b', row=7})) `shouldThrow` errorCall "Input positions are not valid"

    it "input positions not on a horizontal, vertical or diagonal line" $
        evaluate (line (Pos {col='b', row=7}) (Pos {col='c', row=9})) `shouldThrow` errorCall "Input positions are not on a horizontal, vertical or diagonal line"

    it "does not move" $ do
        line (Pos {col='b', row=7}) (Pos {col='b', row=7}) `shouldBe` [Pos {col='b', row=7}]

    it "left to right" $ do
        line (Pos {col='b', row=7}) (Pos {col='e', row=7}) `shouldBe` [Pos {col='b', row=7},
                                                                       Pos {col='c', row=7},
                                                                       Pos {col='d', row=7},
                                                                       Pos {col='e', row=7}]

    it "right to left" $ do
        line (Pos {col='e', row=7}) (Pos {col='b', row=7}) `shouldBe` [Pos {col='e', row=7},
                                                                       Pos {col='d', row=7},
                                                                       Pos {col='c', row=7},
                                                                       Pos {col='b', row=7}]

    it "diagonal upper left to lower right" $ do
        line (Pos {col='b', row=7}) (Pos {col='e', row=4}) `shouldBe` [Pos {col='b', row=7},
                                                                       Pos {col='c', row=6},
                                                                       Pos {col='d', row=5},
                                                                       Pos {col='e', row=4}]

    it "diagonal upper right to lower left" $ do
        line (Pos {col='e', row=7}) (Pos {col='b', row=4}) `shouldBe` [Pos {col='e', row=7},
                                                                       Pos {col='d', row=6},
                                                                       Pos {col='c', row=5},
                                                                       Pos {col='b', row=4}]
