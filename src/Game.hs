module Game
  (
    Status(..),
    State(..),
    runGame,
    originState,
  ) where

import Board (Board)
import qualified Board as Board (empty, checkWin, insertPoint)
import Stone (Stone)
import Side (Side(..))
import qualified Side as Side (next, toStone, originSide)
import Point (Point)

data Status = Continue | Draw | BlackWin | WhiteWin
  deriving (Show, Eq)

data State = State {
  getStatus:: Status,
  getSide:: Side
}

currentStone :: State -> Stone
currentStone = Side.toStone . getSide

winStatus :: Side -> Status
winStatus Black = BlackWin
winStatus White = WhiteWin

runGame :: (Board, State) -> Point -> (Board, State)
runGame (b, s) p = if Board.checkWin newBoard (currentStone s)
    then (newBoard, State { getStatus = winStatus $ getSide s, getSide = getSide s })
    else (newBoard, State { getStatus = Continue, getSide = Side.next $ getSide s })
  where
    newBoard = Board.insertPoint b p (currentStone s)

originStatus :: Status
originStatus = Continue

originState :: State
originState = State { getStatus = originStatus, getSide = Side.originSide}
