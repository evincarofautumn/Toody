{-|
Module      : Toody.Neighborhood
Description : An 8-way neighborhood of cells in a grid.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Toody.Neighborhood
  ( Neighborhood(..)
  , gridNeighborhood
  ) where

import Control.Comonad (Comonad(..))
import Toody.Grid (Grid)
import qualified Toody.Grid as Grid

-- | An 8-way neighborhood extracted from a grid.
data Neighborhood a = Neighborhood
  { neighNorthwest, neighNorth, neighNortheast
  , neighWest, neighSelf, neighEast
  , neighSouthwest, neighSouth, neighSoutheast
    :: a
  } deriving (Eq, Functor, Show)

instance Applicative Neighborhood where
  pure x = Neighborhood
    x x x
    x x x
    x x x

  (<*>) :: forall a b. Neighborhood (a -> b) -> Neighborhood a -> Neighborhood b
  fs <*> xs = let

    apply :: (forall x. Neighborhood x -> x) -> b
    apply direction = direction fs (direction xs)

    in Neighborhood
      { neighNorthwest = apply neighNorthwest
      , neighNorth     = apply neighNorth
      , neighNortheast = apply neighNortheast
      , neighWest      = apply neighWest
      , neighSelf      = apply neighSelf
      , neighEast      = apply neighEast
      , neighSouthwest = apply neighSouthwest
      , neighSouth     = apply neighSouth
      , neighSoutheast = apply neighSoutheast
      }

-- | The neighborhood of the currently focused item in a grid.
--
-- Technically, @'Maybe' ('Neighborhood' a)@ would also work here, but
-- @'Neighborhood' ('Maybe' a)@ is more useful since it tells you which items
-- were successfully extracted.
gridNeighborhood :: Grid a -> Neighborhood (Maybe a)
gridNeighborhood grid = (fmap extract .) <$> Neighborhood
  { neighNorthwest = Grid.northwestward
  , neighNorth     = Grid.northward
  , neighNortheast = Grid.northeastward
  , neighWest      = Grid.westward
  , neighSelf      = pure
  , neighEast      = Grid.eastward
  , neighSouthwest = Grid.southwestward
  , neighSouth     = Grid.southward
  , neighSoutheast = Grid.southeastward
  } <*> pure grid
