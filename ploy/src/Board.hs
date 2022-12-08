module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
import Data.Char
import Data.List.Split
import Data.Function ((&))

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN [] = False
validateFEN xs = (length rows == 9) && (rows & map validateFENRowContent & foldr (&&) True)
    where rows = splitOn "/" xs

validateFENRowContent :: String -> Bool
validateFENRowContent [] = False
validateFENRowContent xs = (length cells == 9) && (cells & map validateFENCellContent & foldr (&&) True)
    where cells = splitOn "," xs

validateFENCellContent :: String -> Bool
validateFENCellContent [] = True
validateFENCellContent [_] = False
validateFENCellContent (x:xs) = elem x ['b', 'w'] && 1 <= xs_int && xs_int <= 255
    where xs_int = read xs :: Int


-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard xs 
    | not (validateFEN xs) = error "Input string is not in FEN notation"
    | otherwise = map (map convertFENCell) cells_list
    where cells_list = xs & splitOn "/" & map (splitOn ",")

convertFENCell :: String -> Cell
convertFENCell [] = Empty
convertFENCell [_] = error "String cell is invalid"
convertFENCell (x:xs) 
    | x == 'b' = Piece Black xs_int
    | x == 'w' = Piece White xs_int
    | otherwise = error "String cell has to start with either 'b' or 'w'"
    where xs_int = read xs :: Int


-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

line :: Pos -> Pos -> [Pos]
line (Pos {col=c1, row=r1}) (Pos {col=c2, row=r2})
    | c1 == c2 && r1 == r2 = [Pos {col=c1, row=r1}]
    | otherwise =
        let 
            col_dist = (ord c2) - (ord c1)
            row_dist = r2 - r1
            col_direction = signum col_dist
            row_direction = signum row_dist
            max_diag = min (abs col_dist) (abs row_dist)
            move_col = if ((abs col_dist) - max_diag) > 0 then 1 else 0
            move_row = if ((abs row_dist) - max_diag) > 0 then 1 else 0
            max_straight = (max (abs col_dist) (abs row_dist)) - max_diag
        in 
            [Pos (chr ((ord c1)+i*col_direction)) (r1+i*row_direction) | i <- [0..max_diag]] ++ 
            [Pos (chr ((ord c2)-i*move_col*col_direction)) (r2-i*move_row*row_direction) | i <- [0..max_straight]]  
