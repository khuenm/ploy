module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift )
import Data.Function ((&))



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

gameFinished :: Board -> Bool
gameFinished b = b & foldr (++) [] & splitBlackWhite & map gameFinishedSingleTeam & foldr (||) False

splitBlackWhite :: [Cell] -> [[Int]]
splitBlackWhite b = [fst blackWhite, snd blackWhite]
    where
    blackWhite = _splitBlackWhite b
    _splitBlackWhite :: [Cell] -> ([Int],[Int])
    _splitBlackWhite [] = ([],[])
    _splitBlackWhite (Empty : xb) = (fst (_splitBlackWhite xb), snd (_splitBlackWhite xb))
    _splitBlackWhite ((Piece Black i) : xb) = (i : fst (_splitBlackWhite xb), snd (_splitBlackWhite xb))
    _splitBlackWhite ((Piece White i) : xb) = (fst (_splitBlackWhite xb), i : snd (_splitBlackWhite xb))

gameFinishedSingleTeam :: [Int] -> Bool
gameFinishedSingleTeam [] = True
gameFinishedSingleTeam xs = xs & map isCommander & _gameFinishedSingleTeam
    where
    _gameFinishedSingleTeam :: [Bool] -> Bool
    _gameFinishedSingleTeam [] = True
    _gameFinishedSingleTeam xs = ((length xs == 1) && head xs) || not (foldr (||) False xs)

isCommander :: Int -> Bool
isCommander n
    | n < 1 || n > 255 = error "Number has to be between 1 and 255 to be converted to binary number"
    | otherwise = sum (toBinary n) == 4

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = _toBinary n
    where
    _toBinary 0 = []
    _toBinary n = mod n 2 : _toBinary (div n 2)

-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

isValidMove :: Board -> Move -> Bool
isValidMove _ _ = False



-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves _ _ = []



-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves _ _ = []
