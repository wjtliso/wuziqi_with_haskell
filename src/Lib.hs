module Lib
  ( runPlay
  ) where

import System.IO
import Board (Board)
import Board as Board (empty, show)
import Side as Side (sideMessage)
import Game (State(..), Status(..))
import Game as Game (runGame, originState, setContinueState, setErrorState)
import Point as Point (fromString)
import Error (Error(..))
import GHC.Base
import Control.Monad

runPlay :: IO ()
runPlay = welcome >> play (Board.empty, Game.originState)

welcome :: IO()
welcome = putStrLn $ unlines
  [ "Wuziqi Version 0.0.1",
    "CopyRight ???"
  ]

play :: (Board, State) -> IO ()
play (b, s) = (putStr $ Board.show b) >> case getStatus s of
  BlackWin -> putStr $ "Black Win !"
  WhiteWin -> putStr $ "White Win !"
  Draw -> putStr $ "Draw !"
  Error -> (putStr $ "Error !\n") >> play (b, Game.setContinueState s)
  _ -> do
    hSetBuffering stdout NoBuffering
    putStr $ Side.sideMessage $ getSide s
    input <- getLine
    let maybePoint = Point.fromString input
    case maybePoint of
      Just point -> play $ runGame (b, s) point
      Nothing -> play (b, Game.setErrorState InputFormatError s)
