module Row
	(
		Row(..),
    next,
    back,
    toString,
    validList,
    fromChar,
	) where

import Range (Range)
import qualified Range as Range (range, next, back, validList, toInt)
import Data.Char (ord, chr)
import Error

newtype Row = Row Range
	deriving (Show, Eq, Ord)

fromChar :: Char -> Either Error Row
fromChar c = case Range.range (ord c - ord 'A' + 1) of
  Just r -> Right $ Row r
  Nothing -> Left RowFormatError

next :: Row -> Maybe Row
next (Row r) = fmap Row (Range.next r)

back :: Row -> Maybe Row
back (Row r) = fmap Row (Range.back r)

validList :: [Row]
validList = fmap Row Range.validList

toString :: Row -> String
toString (Row r) = [ chr (ord 'A' + Range.toInt r - 1) ]