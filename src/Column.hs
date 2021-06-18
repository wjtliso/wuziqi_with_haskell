module Column
	(
		Column(..),
    next,
    back,
    validList,
    toInt,
    toString,
    fromChar,
	) where

import Range (Range)
import qualified Range as Range (range, next, back, validList, toInt)
import Data.Char (ord, chr)
import Error

newtype Column = Column Range
	deriving (Show, Eq, Ord)

fromChar :: Char -> Either Error Column
fromChar c = case Range.range (ord c - ord 'a' + 1) of
  Just c -> Right $ Column c
  Nothing -> Left ColumnFormatError

next :: Column -> Maybe Column
next (Column c) = fmap Column (Range.next c)

back :: Column -> Maybe Column
back (Column c) = fmap Column (Range.back c)

validList :: [Column]
validList = fmap Column Range.validList

toInt :: Column -> Int
toInt (Column c) = Range.toInt c

toString :: Column -> String
toString (Column c) = [ chr (ord 'a' + Range.toInt c - 1) ]
