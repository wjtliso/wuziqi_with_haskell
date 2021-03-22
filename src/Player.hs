module Player
  ( Player(..),
    playerMark
  ) where

data Player = Black | White
  deriving (Show, Eq)

playerMark :: Maybe Player -> String
playerMark (Just Black) = "X"
playerMark (Just White) = "O"
playerMark Nothing = " "