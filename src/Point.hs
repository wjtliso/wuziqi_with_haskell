module Point
	(
		Point(..)
	) where

import Range

data Point = Point Range Range
	deriving (Show, Eq, Ord)

