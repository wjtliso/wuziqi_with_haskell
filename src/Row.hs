module Row
	(
		Row(..),
		fromInt,
    next,
    back,
    toString,
    validList,
    fromChar,
	) where

import Range (Range)
import qualified Range as Range (range, next, back, validList, toInt)
import Data.Char (ord, chr)

newtype Row = Row Range
	deriving (Show, Eq, Ord)

fromInt :: Int -> Maybe Row
fromInt i = fmap Row (Range.range i)

fromChar :: Char -> Maybe Row
fromChar c = fmap Row (Range.range (ord c - ord 'A' + 1))

next :: Row -> Maybe Rowb
next (Row r) = fmap Row (Range.next r)

back :: Row -> Maybe Row
back (Row r) = fmap Row (Range.back r)

validList :: [Row]
validList = fmap Row Range.validList

toString :: Row -> String
toString (Row r) = [ chr (ord 'A' + Range.toInt r - 1) ]