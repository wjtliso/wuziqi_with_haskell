module Error
  (
    Error(..),
  ) where

data Error = InputFormatError | PointExistError deriving (Show)