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

import Control.Exception
import Data.Char (ord, chr)

import Range (Range)
import qualified Range as Range (range, next, back, validList, toInt)

data ColumnFormatException = ColumnFormatException deriving Show
instance Exception ColumnFormatException

newtype Column = Column Range
	deriving (Show, Eq, Ord)

fromChar :: Char -> Column
fromChar c = case Range.range (ord c - ord 'a' + 1) of
  Just c -> Column c
  Nothing -> throw ColumnFormatException

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
