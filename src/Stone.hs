module Stone
  ( Stone(..),
    toMark,
  ) where

data Stone = Black | White
  deriving (Show, Eq)

toMark :: Stone -> String
toMark Black = "X"
toMark White = "O"
