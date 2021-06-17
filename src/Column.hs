module Column
	(
		Column(..),
		fromInt,
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

newtype Column = Column Range
	deriving (Show, Eq, Ord)

fromInt :: Int -> Maybe Column
fromInt i = fmap Column (Range.range i)

fromChar :: Char -> Maybe Column
fromChar c = fmap Column (Range.range (ord c - ord 'a' + 1))

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
