module Range
  (
    Range,
    range,
    range,
    next,
    back,
  ) where

data Range = Range Int
  deriving (Show, Eq, Ord)

max :: Int
max = 19

min :: Int
min = 1

between :: Int -> Bool
between i = if i <= Range.max && i >= Range.min
  then True
  else False

range :: Int -> Maybe Range
range i = if Range.between i
  then Just $ Range i
  else Nothing

next :: Range -> Maybe Range
next (Range i) = if i < Range.max
  then Just $ Range (i + 1)
  else Nothing

back :: Range -> Maybe Range
back (Range i) = if i > Range.min
  then Just $ Range (i - 1)
  else Nothing
