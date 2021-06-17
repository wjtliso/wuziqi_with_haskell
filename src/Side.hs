module Side
  ( Side(..),
    next,
    toStone,
    originSide,
    sideMessage,
  ) where

import Stone (Stone(..))

data Side = Black | White
  deriving (Show, Eq)

next :: Side -> Side
next Side.Black = Side.White
next Side.White = Side.Black

toStone :: Side -> Stone
toStone Side.Black = Stone.Black
toStone Side.White = Stone.White

originSide :: Side
originSide = Side.Black

sideMessage :: Side -> String
sideMessage Side.Black = "> Black side move (X)\n"
sideMessage Side.White = "> White side move (O)\n"