{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Toody
  ( Grid(..)
  , Neighborhood(..)
  , Zipper(..)
  , eastward
  , leftward
  , makeGrid
  , makeZipper
  , neighborhood
  , northeastward
  , northward
  , northwestward
  , rightward
  , southeastward
  , southward
  , southwestward
  , westward
  , zipperIndex
  , zipperLength
  ) where

import Control.Comonad (Comonad(..))
import Control.Monad ((<=<))
import Data.Function (on)
import Data.List (intercalate)

-- | A 1D zipper, representing a sequence of values with a focus.
data Zipper a = Zipper
  { zipperBefore  :: [a]
  , zipperCurrent :: a
  , zipperAfter   :: [a]
  } deriving (Eq, Foldable, Functor, Traversable)

-- | Motion within a zipper.
rightward, leftward :: Zipper a -> Maybe (Zipper a)

rightward (Zipper b c (n : a)) = Just (Zipper (c : b) n a)
rightward _ = Nothing

leftward (Zipper (p : b) c a) = Just (Zipper b p (c : a))
leftward _ = Nothing

instance Comonad Zipper where
  extract = zipperCurrent
  duplicate z = let
    index = zipperIndex z
    before = reverse (iterateMaybeN index leftward (leftward z))
    after = iterateMaybeN (zipperLength z - index - 1) rightward (rightward z)
    in Zipper before z after

iterateMaybeN :: Int -> (a -> Maybe a) -> Maybe a -> [a]
iterateMaybeN n f = take n . iterateMaybe f

iterateMaybe :: (a -> Maybe a) -> Maybe a -> [a]
iterateMaybe f = go
  where
    go (Just x) = x : go (f x)
    go Nothing = []

-- | Get the number of elements in a zipper.
zipperLength :: Zipper a -> Int
zipperLength z = length (zipperBefore z) + 1 + length (zipperAfter z)

-- | Get the current cursor offset within a zipper, starting from 0.
zipperIndex = length . zipperBefore

-- | A 2D zipper representing a grid of values with a focus.
newtype Grid a = Grid { unGrid :: Zipper (Zipper a) }
  deriving (Eq, Functor)

-- | Motion within a grid.
northwestward, northward, northeastward,
  westward, eastward,
  southwestward, southward, southeastward
  :: Grid a -> Maybe (Grid a)

northward = fmap Grid .          leftward  . unGrid
southward = fmap Grid .          rightward . unGrid
westward  = fmap Grid . traverse leftward  . unGrid
eastward  = fmap Grid . traverse rightward . unGrid

northwestward = northward <=< westward
northeastward = northward <=< eastward
southwestward = southward <=< westward
southeastward = southward <=< eastward

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
neighborhood :: Grid a -> Neighborhood (Maybe a)
neighborhood grid = (fmap extract .) <$> Neighborhood
  { neighNorthwest = northwestward
  , neighNorth     = northward
  , neighNortheast = northeastward
  , neighWest      = westward
  , neighSelf      = pure
  , neighEast      = eastward
  , neighSouthwest = southwestward
  , neighSouth     = southward
  , neighSoutheast = southeastward
  } <*> pure grid

-- | Create a zipper from a list, using a default element as the focus if the
-- list is empty.
makeZipper :: a -> [a] -> Zipper a
makeZipper _ (x : xs) = Zipper [] x xs
makeZipper p []       = Zipper [] p []

-- | Create a grid from a list of lists, using a default element as the focus if
-- there are no rows or columns, as well as to pad all rows to the same width.
makeGrid :: a -> [[a]] -> Grid a
makeGrid p rows0 = let
  z0 = makeZipper p []
  in case rows0 of
    row : rows -> let
      width = maximum (map length rows0)
      pad = take width . (++ repeat p)
      in Grid (makeZipper z0 (map (makeZipper p . pad) rows0))
    [] -> Grid (makeZipper z0 [z0])

-- Debugging utilities.

instance (Show a) => Show (Zipper a) where
  show z = concat
    [ unwords (map show (reverse (zipperBefore z)))
    , "(", show (zipperCurrent z), ")"
    , unwords (map show (zipperAfter z))
    ]

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
