module Board
  (
    Board,
    empty,
    checkWin,
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe
import Player
import Error
import qualified Range as R
import Point

data Board = Board (M.Map Point Player)

empty :: Board
empty = Board M.empty

isExist :: Board -> Point -> Bool
isExist (Board b) p = case M.lookup p b of
  Just _ -> True
  _ -> False

getPlayer :: Board -> Point -> Maybe Player
getPlayer (Board b) p = M.lookup p b

checkWin :: Board -> Point -> Player -> Bool
checkWin b po pl = foldr (\(f1, f2) re -> if re then re else isNPointContinue b po pl (f1, f2) 5) False [(R.next, Just), (R.next, R.next), (R.next, R.back), (Just, R.next), (R.back, Just), (R.back, R.back), (R.back, R.next), (Just, R.back)]

isNPointContinue :: Board -> Point　-> Player -> (R.Range -> Maybe R.Range, R.Range -> Maybe R.Range) -> Int -> Bool
isNPointContinue b (Point c r) p (f1, f2) n =
  if n == 0
  then True
  else case getPlayer b (Point c r) of
    Nothing -> False
    Just _p -> if _p == p
      then case (f1 c, f2 r) of
        (Just cc, Just rr) -> isNPointContinue b (Point cc rr) p (f1, f2) (n - 1)
        _ -> False
      else False
