module Lib
  ( runPlay
  ) where

import System.IO
import Control.Exception

import Board (Board)
import qualified Board as Board (empty, show)
import Side as Side (sideMessage)
import Game (State(..), Status(..))
import Game as Game (runGame, originState)
import Point (Point)
import qualified Point as Point (fromString)

runPlay :: IO ()
runPlay = welcome >> play (Board.empty, Game.originState)

welcome :: IO()
welcome = putStrLn $ unlines
  [ "Wuziqi Version 0.0.1",
    "CopyRight ???"
  ]

play :: (Board, State) -> IO ()
play (b, s) = (putStr $ Board.show b) >> case getStatus s of
  BlackWin -> putStrLn "Black Win !"
  WhiteWin -> putStrLn "White Win !"
  Draw -> putStrLn "Draw !"
  _ -> do
    hSetBuffering stdout NoBuffering
    putStrLn $ Side.sideMessage $ getSide s
    point <- exceptionOrPoint
    play $ runGame (b, s) point

getPoint :: IO Point
getPoint = do
  input <- getLine
  pure $ Point.fromString input

exceptionOrPoint :: IO Point
exceptionOrPoint = catch getPoint
  (\e -> do
    let err = show (e :: IOException)
    putStrLn err
    getPoint
  )