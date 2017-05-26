{-|
Module      : Toody.Box
Description : A point in a grid.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

module Toody.Point
  ( Column
  , Point(..)
  , Row
  , humanLocation
  ) where

import Data.Monoid ((<>))

-- | A point in a grid.
data Point = Point !Row !Column
  deriving (Eq, Show)

-- | Horizontal offset in cells.
type Row = Int

-- | Vertical offset in cells.
type Column = Int

-- | Converts a 'Point' to a human-readable prefix.
humanLocation :: Point -> String
humanLocation (Point row column) = show row <> ":" <> show column
