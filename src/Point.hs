module Point
	(
		Point(..),
		nextOfSameColumn,
		backOfSameColumn,
		nextOfSameRow,
		backOfSameRow,
    nextColumnNextRow,
    nextColumnBackRow,
    backColumnNextRow,
    backColumnBackRow,
    allOfSameColumn,
    fromString,
	) where

import Column (Column)
import qualified Column as Column (next, back, fromChar)
import Row (Row)
import qualified Row as Row (next, back, validList, fromChar)
import Control.Applicative
import Error

data Point = Point Column Row
	deriving (Show, Eq, Ord)

nextOfSameColumn :: Point -> Maybe Point
nextOfSameColumn p = nextPoint p Just Row.next

backOfSameColumn :: Point -> Maybe Point
backOfSameColumn p = nextPoint p Just Row.back

nextOfSameRow :: Point -> Maybe Point
nextOfSameRow p = nextPoint p Column.next Just

backOfSameRow :: Point -> Maybe Point
backOfSameRow p = nextPoint p Column.back Just

nextColumnNextRow :: Point -> Maybe Point
nextColumnNextRow p = nextPoint p Column.next Row.next

nextColumnBackRow :: Point -> Maybe Point
nextColumnBackRow p = nextPoint p Column.next Row.back

backColumnNextRow :: Point -> Maybe Point
backColumnNextRow p = nextPoint p Column.back Row.next

backColumnBackRow :: Point -> Maybe Point
backColumnBackRow p = nextPoint p Column.back Row.back

nextPoint :: Point -> (Column -> Maybe Column) -> (Row -> Maybe Row) -> Maybe Point
nextPoint (Point c r) f g = liftA2 Point (f c) (g r)

allOfSameColumn :: Column -> [Point]
allOfSameColumn c = fmap (Point c) Row.validList

fromString :: String -> Either Error Point
fromString s = case s of
  c : r : [] -> liftA2 Point (Column.fromChar c) (Row.fromChar r)
  _ -> Left InputFormatError

