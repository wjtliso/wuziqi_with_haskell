module Board
  (
    Board,
    printColumn,
    emptyBoard,
    showBoard,
    insertStringToBoard,
    checkWin,
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe
import Player
import Column
import Row
import Error
import Data.Char
import Data.Profunctor.Product.Flatten

data Board = Board (M.Map Column (M.Map Row Player))

emptyBoard :: Board
emptyBoard = Board M.empty

insertStringToBoard :: Board -> String -> Player -> Either Error Board
insertStringToBoard board s p= case s of
  (a : b : []) -> case (column a, row b) of
    (Just c, Just r) -> if existInBoard board c r
      then Left PointExistError
      else Right $ insertBoard board c r p
    _ -> Left InputFormatError
  _ -> Left InputFormatError

insertBoard :: Board -> Column -> Row -> Player -> Board
insertBoard (Board b) c r p = case loadRow (Board b) c of
  Nothing -> Board $ M.insert c (M.singleton r p) b
  Just rs -> Board $ M.update (\rs -> Just (M.insert r p rs)) c b

existInBoard :: Board -> Column -> Row -> Bool
existInBoard b c r = case getPlayer b (Just c) (Just r) of
  Nothing -> False
  _ -> True

getPlayer :: Board -> Maybe Column -> Maybe Row -> Maybe Player
getPlayer b c r = case (c, r) of
  (Just cc, Just rr) -> case loadRow b cc of
    Nothing -> Nothing
    Just rs -> M.lookup rr rs
  _ -> Nothing

loadRow :: Board -> Column -> Maybe(M.Map Row Player)
loadRow (Board b) c = M.lookup c b

printColumn :: Board -> Column -> String
printColumn b c = case loadRow b c of
  Nothing -> "                   "
  Just rs -> foldr (\a b -> b ++ " " ++ (playerMark (M.lookup a rs))) "" (catMaybes $ fmap row (reverse rowCharSet))

showBoard :: Board -> String
showBoard board = "  a b c d e f g h i j k l m n o p q r s t" ++ (foldr (\a b -> b ++ [columnToChar a] ++ (printColumn board a) ++ "\n") "\n" (catMaybes $ fmap column (reverse columnCharSet)))

checkWin :: Board -> Player -> Bool
checkWin b p = any (\(c,r) -> any (\f -> f b p (Just c) (Just r) 5) [checkEast, checkWest, checkNorth, checkSouth, checkNorthEast, checkSouthEast, checkSouthEast, checkSouthWest]) (pointOfPlayer b p)

checkEast :: Board -> Player -> Maybe Column -> Maybe Row -> Int -> Bool
checkEast b p mc mr i = if i == 0
  then True
  else case getPlayer b mc mr of
    Just _p -> if _p == p
      then checkEast b p mc (nextRow mr) (i - 1)
      else False
    _ -> False

checkWest :: Board -> Player -> Maybe Column -> Maybe Row -> Int -> Bool
checkWest b p mc mr i = if i == 0
  then True
  else case getPlayer b mc mr of
    Just _p -> if _p == p
      then checkWest b p mc (backRow mr) (i - 1)
      else False
    _ -> False

checkNorth :: Board -> Player -> Maybe Column -> Maybe Row -> Int -> Bool
checkNorth b p mc mr i = if i == 0
  then True
  else case getPlayer b mc mr of
    Just _p -> if _p == p
      then checkNorth b p (backColumn mc) mr (i - 1)
      else False
    _ -> False

checkSouth :: Board -> Player -> Maybe Column -> Maybe Row -> Int -> Bool
checkSouth b p mc mr i = if i == 0
  then True
  else case getPlayer b mc mr of
    Just _p -> if _p == p
      then checkSouth b p (nextColumn mc) mr (i - 1)
      else False
    _ -> False

checkNorthEast :: Board -> Player -> Maybe Column -> Maybe Row -> Int -> Bool
checkNorthEast b p mc mr i = if i == 0
  then True
  else case getPlayer b mc mr of
    Just _p -> if _p == p
      then checkNorthEast b p (backColumn mc) (nextRow mr) (i - 1)
      else False
    _ -> False

checkSouthEast :: Board -> Player -> Maybe Column -> Maybe Row -> Int -> Bool
checkSouthEast b p mc mr i = if i == 0
  then True
  else case getPlayer b mc mr of
    Just _p -> if _p == p
      then checkSouthEast b p (nextColumn mc) (nextRow mr) (i - 1)
      else False
    _ -> False

checkNorthWest :: Board -> Player -> Maybe Column -> Maybe Row -> Int -> Bool
checkNorthWest b p mc mr i = if i == 0
  then True
  else case getPlayer b mc mr of
    Just _p -> if _p == p
      then checkNorthWest b p (backColumn mc) (backRow mr) (i - 1)
      else False
    _ -> False

checkSouthWest :: Board -> Player -> Maybe Column -> Maybe Row -> Int -> Bool
checkSouthWest b p mc mr i = if i == 0
  then True
  else case getPlayer b mc mr of
    Just _p -> if _p == p
      then checkSouthWest b p (nextColumn mc) (backRow mr) (i - 1)
      else False
    _ -> False

toList :: Board -> [(Column, Row, Player)]
toList b = concat $ fmap (\c -> rowToList b c) (catMaybes $ fmap column columnCharSet)

rowToList :: Board -> Column -> [(Column, Row, Player)]
rowToList (Board b) c = case M.lookup c b of
  Just rs -> fmap (\a -> flatten3 (c, a) ) (M.toList rs)
  _ -> []

pointOfPlayer :: Board -> Player -> [(Column, Row)]
pointOfPlayer b p = foldr (\(c,r,_p) b -> if _p == p then (c,r) : b else b) [] (toList b)
