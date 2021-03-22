module Column
  (
    Column(),
    column,
    columnCharSet,
    columnToChar,
    backColumn,
    nextColumn,
  ) where

import Data.Char

columnCharSet = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T']

data Column = Column Char 
    deriving (Show, Eq, Ord)

column :: Char -> Maybe Column
column a = if elem a columnCharSet 
           then Just $ Column a 
           else Nothing

nextColumn :: Maybe Column -> Maybe Column
nextColumn mc = case mc of
  Just (Column c) -> column $ chr (ord c + 1)
  _ -> Nothing

backColumn :: Maybe Column -> Maybe Column
backColumn mc = case mc of
  Just (Column c) -> column $ chr (ord c - 1)
  _ -> Nothing

columnToChar :: Column -> Char
columnToChar (Column c) = c
