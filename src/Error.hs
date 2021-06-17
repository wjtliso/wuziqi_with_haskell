module Error
  (
    Error(..),
  ) where

data Error = InputFormatError | PointExistError | NoError
  deriving (Show)