{-|
Module      : Toody.Grid
Description : A 2D zipper representing a grid of cells with a focus.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Toody.Grid
  ( Grid(..)
  , Move
  , eastward
  , gridFrom
  , gridLocation
  , northeastward
  , northward
  , northwestward
  , southeastward
  , southward
  , southwestward
  , westward
  ) where

import Control.Arrow ((&&&))
import Control.Comonad (Comonad(..))
import Control.Monad ((<=<))
import Data.Foldable (Foldable(..))
import Data.List (intercalate)
import Toody.Point
import Toody.Utils
import Toody.Zipper

-- | A 2D zipper representing a grid of values with a focus.
newtype Grid a = Grid { unGrid :: Zipper (Zipper a) }
  deriving (Eq, Foldable, Functor, Traversable)

-- | A motion within a grid that may fail, for example if it goes out of bounds.
type Move a = Grid a -> Maybe (Grid a)

-- | Standard motions within a grid.
northwestward, northward, northeastward,
  westward, eastward,
  southwestward, southward, southeastward
  :: Move a

northward = fmap Grid .          leftward  . unGrid
southward = fmap Grid .          rightward . unGrid
westward  = fmap Grid . traverse leftward  . unGrid
eastward  = fmap Grid . traverse rightward . unGrid

northwestward = northward <=< westward
northeastward = northward <=< eastward
southwestward = southward <=< westward
southeastward = southward <=< eastward

-- | Create a grid from a list of lists, using a default element as the focus if
-- there are no rows or columns, as well as to pad all rows to the same width.
gridFrom :: a -> [[a]] -> Grid a
gridFrom p rows0 = let
  z0 = zipperFrom p []
  in if null rows0
    then Grid (zipperFrom z0 [z0])
    else let
      width = maximum (map length rows0)
      pad = take width . (++ repeat p)
      in Grid (zipperFrom z0 (map (zipperFrom p . pad) rows0))

-- | Extracts the current horizontal and vertical offset.
gridLocation :: Grid a -> Point
gridLocation = uncurry Point . (zipperIndex &&& zipperIndex . zipperCurrent) . unGrid

instance Comonad Grid where

  extract :: Grid a -> a
  extract = extract . extract . unGrid

  duplicate :: forall a. Grid a -> Grid (Grid a)
  duplicate grid@(Grid z) = let

    focusInner :: Zipper (Grid a)
    focusInner = let
      befores = innerBefores grid
      afters = innerAfters grid
      in Zipper befores grid afters

    outerIndex, outerLength, innerIndex, innerLength :: Int
    outerIndex  = zipperIndex z
    innerIndex  = zipperIndex (extract z)
    outerIndex' = outerLength - outerIndex - 1
    innerIndex' = innerLength - innerIndex - 1
    outerLength = zipperLength z
    innerLength = zipperLength (extract z)

    outerBefores, outerAfters :: Zipper (Grid a) -> [Zipper (Grid a)]
    outerBefores = reverse . iterateMaybeN outerIndex  (traverse northward) . traverse northward
    outerAfters  =           iterateMaybeN outerIndex' (traverse southward) . traverse southward

    innerBefores, innerAfters :: Grid a -> [Grid a]
    innerBefores = reverse . iterateMaybeN innerIndex  westward . westward
    innerAfters  =           iterateMaybeN innerIndex' eastward . eastward

    in Grid
      (Zipper
        (outerBefores focusInner)
        focusInner
        (outerAfters focusInner))

-- Debugging utilities.

instance (Show a) => Show (Grid a) where
  show (Grid z) = let
    before = map show (reverse (zipperBefore z))
    after = map show (zipperAfter z)
    width = case map length (before ++ after) of
      [] -> 1
      lengths -> maximum lengths
    sep = concat ["\n", replicate width '-', "\n"]
    in concat
      [ "\n", intercalate "\n" before
      , sep, show (zipperCurrent z), sep
      , intercalate "\n" after, "\n"
      ]
