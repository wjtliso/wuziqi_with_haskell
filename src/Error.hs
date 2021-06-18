module Error
  (
    Error(..),
  ) where

data Error = RowFormatError | ColumnFormatError | InputFormatError | PointExistError
  deriving (Show)