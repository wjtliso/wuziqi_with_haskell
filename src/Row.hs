module Row
  (
    Row(),
    row,
    rowCharSet,
    rowToChar,
    nextRow,
    backRow,
  ) where

import Data.Char

rowCharSet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't']

data Row = Row Char 
    deriving (Show, Eq, Ord)

row :: Char -> Maybe Row
row a = if elem a rowCharSet 
        then Just $ Row a 
        else Nothing

nextRow :: Maybe Row -> Maybe Row
nextRow mr = case mr of
  Just (Row r) -> row $ chr (ord r + 1)
  _ -> Nothing

backRow :: Maybe Row  -> Maybe Row
backRow mr = case mr of
  Just (Row r) -> row $ chr (ord r - 1)
  _ -> Nothing

rowToChar :: Row -> Char
rowToChar (Row r) = r
