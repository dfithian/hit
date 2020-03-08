module Types where

import ClassyPrelude hiding (FilePath)
import Control.Lens.TH (makePrisms)
import Turtle (FilePath)

data Location
  = LocationAll
  | LocationSubdir FilePath
  deriving (Eq, Show)

makePrisms ''Location
