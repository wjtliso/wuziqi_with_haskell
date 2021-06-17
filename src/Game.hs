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
import Error (Error(..))

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

runGame :: (Board, State) -> Point -> Either Error (Board, State)
runGame (b, s) p = case Board.insertPoint b p (currentStone s) of
    Right bb -> if Board.checkWin bb (currentStone s)
      then Right (bb, State { getStatus = winStatus $ getSide s, getSide = getSide s })
      else Right (bb, State { getStatus = Continue, getSide = Side.next $ getSide s })
    Left e -> Left e

originStatus :: Status
originStatus = Continue

originState :: State
originState = State { getStatus = originStatus, getSide = Side.originSide}
