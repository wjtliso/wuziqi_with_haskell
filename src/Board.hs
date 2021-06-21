module Board
  (
    Board,
    empty,
    show,
    checkWin,
    insertPoint,
  ) where

import Prelude hiding (show)
import qualified Data.Map.Strict as M
import Data.Char (ord, chr)
import Data.Maybe
import Control.Exception

import Stone (Stone)
import qualified Stone as Stone (toMark)
import Point (Point(..))
import Column (Column)
import qualified Column as Column (validList, toInt, toString)
import Row (Row)
import qualified Row as Row (validList, toString)
import qualified Point as Point (
  nextOfSameColumn,
  backOfSameColumn,
  nextOfSameRow,
  backOfSameRow,
  nextColumnNextRow,
  nextColumnBackRow,
  backColumnNextRow,
  backColumnBackRow,
  allOfSameColumn)
import qualified Range as Range (max)
import qualified Range as Range (validList, toInt)

data Board = Board (M.Map Point Stone)
  deriving (Show)

data PointExistException = PointExistException deriving Show
instance Exception PointExistException

empty :: Board
empty = Board M.empty

isExist :: Board -> Point -> Bool
isExist (Board b) p = case M.lookup p b of
  Just _ -> True
  _ -> False

getStone :: Board -> Point -> Maybe Stone
getStone (Board b) p = M.lookup p b

checkWin :: Board -> Stone -> Bool
checkWin b s = foldr (\p re -> if re then re else checkWinByPoint b p s) False points
  where
    points = pointsOfStone b s

pointsOfStone :: Board -> Stone -> [Point]
pointsOfStone (Board b) s = map (\(p, _s) -> p) pointStoneList
  where
    pointStoneList = filter (\(p, _s) -> _s == s) (M.toList b)

checkWinByPoint :: Board -> Point -> Stone -> Bool
checkWinByPoint b po s = foldr (\f re -> if re then re else isNPointContinue b po s f 5) False [Point.nextOfSameColumn, Point.backOfSameColumn, Point.nextOfSameRow, Point.backOfSameRow, Point.nextColumnNextRow, Point.nextColumnBackRow, Point.backColumnNextRow, Point.backColumnBackRow]

isNPointContinue :: Board -> Point　-> Stone -> (Point -> Maybe Point) -> Int -> Bool
isNPointContinue b p s f n =
  if n == 0
  then True
  else
    if isSameStone b p s
    then case f p of
      Just pp -> isNPointContinue b pp s f (n - 1)
      _ -> False
    else False
  where
    isSameStone :: Board -> Point -> Stone -> Bool
    isSameStone b p s = case getStone b p of
      Nothing -> False
      Just _s -> _s == s

insertPoint :: Board -> Point -> Stone -> Board
insertPoint (Board b) p s =
  if isExist (Board b) p
  then throw PointExistException
  else Board $ M.insert p s b

show :: Board -> String
show board = header ++ "\n" ++ foldr (\c b -> b ++ columnToStr board c ++ "\n") "" Column.validList

header :: String
header = foldr (\a b-> b ++ " " ++ Row.toString a ) "" Row.validList

columnToStr :: Board -> Column -> String
columnToStr board c = Column.toString c ++ foldr (\point b -> b ++ (getPointMark board point) ++ " ") "" (Point.allOfSameColumn c)

getPointMark :: Board -> Point -> String
getPointMark b p = case getStone b p of
  Just s -> Stone.toMark s
  Nothing -> " "