{-|
Module      : Toody.Box
Description : Representation of a box with location and size.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

module Toody.Box
  ( Box(..)
  , Point(..)
  , Size(..)
  ) where

import Toody.Point (Point(..))
import Toody.Size (Size(..))

-- | A box within a grid, used for reporting source spans in error messages and
-- extracting sub-grids from grids.
data Box = Box !Point !Size
  deriving (Eq, Show)
