module Lib
  ( runPlay
  ) where

import System.IO
import Board (Board)
import Board as Board (empty, show)
import Side as Side (sideMessage)
import Game (State(..), Status(..))
import Game as Game (runGame, originState)
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
  _ -> do
    hSetBuffering stdout NoBuffering
    putStr $ Side.sideMessage $ getSide s
    input <- getLine
    let maybePoint = Point.fromString input
    case maybePoint of
      Just point -> case runGame (b, s) point of
        Right bs -> play bs
        Left e -> (putStr $ (Prelude.show e) ++ "\n" ) >> play (b, s)
      Nothing -> (putStr $ "Error !\n") >> play (b, s)
