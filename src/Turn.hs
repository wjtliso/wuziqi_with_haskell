module Turn
  (
    Status,
    Turn(..),
    runTurn
  ) where

import qualified Board as B
import Player
import Point

data Status = Continue | Draw | Win
  deriving (Show, Eq)

data Turn = Turn B.Board Player Status
  deriving (Show)

runTurn :: Turn -> Point -> Turn
runTurn (Turn b player s) point = if B.checkWin b point player
  then Turn b player Win
  else Turn b (nextPlayer player) Continue
