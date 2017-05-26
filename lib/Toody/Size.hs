{-|
Module      : Toody.Size
Description : The extents of a region in a grid.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

module Toody.Size
  ( Height
  , Size(..)
  , Width
  ) where

-- | The extents of a region in a grid.
data Size = Size !Width !Height
  deriving (Eq, Show)

-- | Width in cells. Should be non-negative.
type Width = Int

-- | Height in cells. Should be non-negative.
type Height = Int
