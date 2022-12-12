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

-- instance Show Pos where
--   show Pos {col=c, row=r} = c : show r

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
    where 
        cells_list = xs & splitOn "/" & map (splitOn ",")
        convertFENCell :: String -> Cell
        convertFENCell [] = Empty
        convertFENCell (x:xs) 
            | x == 'b' = Piece Black xs_int
            | x == 'w' = Piece White xs_int
            where xs_int = read xs :: Int


-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

line :: Pos -> Pos -> [Pos]
line (Pos {col=c1, row=r1}) (Pos {col=c2, row=r2})
    | c1 == c2 && r1 == r2 = [Pos {col=c1, row=r1}]
    | r2 == r1 = [Pos {col=chr ((ord c1)+i*col_direction), row=r1} | i <- [0..(abs col_dist)]]
    | ord c1 == ord c2 = [Pos {col=c1, row=r1+i*row_direction} | i <- [0..(abs row_dist)]]
    | otherwise = [Pos {col=chr ((ord c1)+i*col_direction), row=r1+i*row_direction} | i <- [0..(abs col_dist)]]
    where
        col_dist = (ord c2) - (ord c1)
        row_dist = r2 - r1
        col_direction = signum col_dist
        row_direction = signum row_dist
