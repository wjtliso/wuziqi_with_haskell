module Lib
  ( runPlay
  ) where

import System.IO
import Board
import Player
import GHC.Base
import Control.Monad

runPlay :: IO ()
runPlay = welcome >> play emptyBoard

welcome :: IO()
welcome = putStrLn $ unlines
  [ "Wuziqi Version 0.0.1",
    "CopyRight ???"
  ]

play :: Board -> IO ()
play b = do
  hSetBuffering stdout NoBuffering 
  putStr "> White trun (O)"
  winput <- getLine
  wb <- insertIntoBoard b winput White
  -- TODO:判断和棋
  guard (not $ checkWin wb White)
  putStr "> Black trun (X)"
  binput <- getLine
  bb <- insertIntoBoard wb binput Black
  guard (not $ checkWin bb Black)
  play bb

insertIntoBoard :: Board -> String -> Player -> IO Board
insertIntoBoard b s p = case insertStringToBoard b s p of
  Left e -> do
    putStr "> Input error! Try again!"
    input <- getLine
    insertIntoBoard b input p
  Right nb -> do
    putStr $ Board.showBoard nb
    returnIO nb
