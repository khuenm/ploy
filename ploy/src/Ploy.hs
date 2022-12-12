module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
import Data.Char
import Data.Bits ( (.&.), (.|.), shift )
import Data.Function ((&))
import Data.List (nub, delete)

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

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = _toBinary n
    where
    _toBinary 0 = []
    _toBinary n = mod n 2 : _toBinary (div n 2)

toTuple3 :: [a] -> (a,a,a)
toTuple3 [x,y,z] = (x,y,z)

-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

gameFinished :: Board -> Bool
gameFinished b = b & splitBlackWhite & map gameFinishedSingleTeam & foldr (||) False

splitBlackWhite :: Board -> [[Int]]
splitBlackWhite b = [fst blackWhite, snd blackWhite]
    where
    blackWhite = _splitBlackWhite (foldr (++) [] b) -- Flatten Board to [Cell]
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


-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

-- isValidMove only checks if the path between start and target pos is empty
-- target pos is not considered since there is no information about which player is playing
isValidMove :: Board -> Move -> Bool
isValidMove b (Move {start=p1, target=p2, turn=_}) 
    | p1 == p2 = True
    | otherwise = line p1 p2 & tail & init & isValidPos b

isValidPos :: Board -> [Pos] -> Bool
isValidPos _ [] = True -- No move since start and target is the same
isValidPos b (pos : xs) = (getCell b pos == Empty) && (isValidPos b xs)

getCell :: Board -> Pos -> Cell
getCell b (Pos {col=c, row=r}) = b!!(9-r)!!((ord c)-97)

-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves _ Empty = []
possibleMoves p (Piece _ i) = i & toBinary & generateAllMoves & concatenateStartPos p

-- (col, row, turn)
generateAllMoves :: [Int] -> [(Int,Int,Int)]
generateAllMoves xs = xs & sum & getAllMoves & map toTuple3 & filterDirections (getPossibleDirections xs)
    where
    getAllMoves :: Int -> [[Int]]
    getAllMoves 1 = [[a,b,c] | a <- [-1,0,1], b <- [-1,0,1], c <- [0,7], sum [a,b,c] > 0 || a /= b || b /= c] -- Shield
    getAllMoves n = ([[a*i1,b*i2,0] | a <- [-1,0,1], 
                                      b <- [-1,0,1], 
                                      i1 <- [1..((div n 4)+(mod n 4))], 
                                      i2 <- [1..((div n 4)+(mod n 4))]
                     ] & nub & delete [0,0,0])
                    ++ [[0,0,i] | i <- [1..7]]

    getPossibleDirections :: [Int] -> [[Int]]
    getPossibleDirections xs = [tail x | x <- [[0,0,1],[1,1,1],[2,1,0],[3,1,-1],[4,0,-1],[5,-1,-1],[6,-1,0],[7,-1,1]], length xs > x!!0, xs!!(x!!0) /= 0]

    filterDirections :: [[Int]] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
    filterDirections possibleDirections allMoves = [(c,r,t) | (c,r,t) <- allMoves, (elem [signum c, signum r] possibleDirections) || (c==0 && r==0)]

concatenateStartPos :: Pos -> [(Int,Int,Int)] -> [Move]
concatenateStartPos (Pos {col=c1, row=r1}) xs = [Move {start=Pos {col=c1, row=r1}, 
                                                       target=Pos {col=chr (i2+ord c1), row=r1+r2}, 
                                                       turn=t
                                                      } 
                                                 | (i2,r2,t) <- xs, 
                                                   i2 + ord c1 - 97 + 1 >= 1,
                                                   i2 + ord c1 - 97 + 1 <= 9,
                                                   r1 + r2 >= 1,
                                                   r1 + r2 <= 9
                                                ]

-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves b player 
    | gameFinished b = []
    | otherwise = b & generatePosCellTuple 
                   & filter (\t -> isPlayer player (snd t)) 
                   & map (uncurry possibleMoves)
                   & foldr (++) []
                   & filter (isValidMove b)
                   & filter (isValidEndPos b player)
    where
    generatePosCellTuple b = [(Pos c r, b!!(9-r)!!((ord c)-97)) | r <- [1..9], c <- ['a'..'i']]

    isPlayer :: Player -> Cell -> Bool
    isPlayer _ Empty = False
    isPlayer player (Piece color _) = player == color

    isValidEndPos :: Board -> Player -> Move -> Bool
    isValidEndPos b player (Move {start=_, target=p2, turn=_}) = not (isPlayer player (getCell b p2)) 

