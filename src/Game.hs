module Game
  (
    Status(..),
    State(..),
    runGame,
    originState,
    setContinueState,
    setErrorState,
  ) where

import Board (Board)
import qualified Board as Board (empty, checkWin, insertPoint)
import Stone (Stone)
import Side (Side(..))
import qualified Side as Side (next, toStone, originSide)
import Point (Point)
import Error (Error(..))

data Status = Continue | Draw | BlackWin | WhiteWin | Error
  deriving (Show, Eq)

data State = State {
  getStatus:: Status,
  getSide:: Side,
  getError:: Error
}

currentStone :: State -> Stone
currentStone = Side.toStone . getSide

winStatus :: Side -> Status
winStatus Black = BlackWin
winStatus White = WhiteWin

setContinueState :: State -> State
setContinueState s = State { getStatus = Continue, getSide = getSide s, getError = NoError }

setErrorState :: Error -> State -> State
setErrorState e s = State { getStatus = Error, getSide = getSide s, getError = e }

runGame :: (Board, State) -> Point -> (Board, State)
runGame (b, s) p = if Board.checkWin b p (currentStone s)
  then (b, State { getStatus = winStatus $ getSide s, getSide = getSide s, getError = NoError })
  else
    case Board.insertPoint b p (currentStone s) of
    Right bb -> (bb, State { getStatus = Continue, getSide = Side.next $ getSide s, getError = NoError })
    Left e -> (b, State { getStatus = Error, getSide = getSide s, getError = e })

originStatus :: Status
originStatus = Continue

originState :: State
originState = State { getStatus = originStatus, getSide = Side.originSide, getError = NoError}
