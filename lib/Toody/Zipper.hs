{-|
Module      : Toody.Zipper
Description : A 1D zipper representing a row of cells with a focus.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Toody.Zipper
  ( Zipper(..)
  , leftward
  , rightward
  , zipperFrom
  , zipperIndex
  , zipperLength
  ) where

import Control.Comonad (Comonad(..))
import Data.Foldable (Foldable(..))
import Toody.Utils (iterateMaybeN)

-- | A 1D zipper, representing a sequence of values with a focus.
data Zipper a = Zipper
  { zipperBefore  :: [a]
  , zipperCurrent :: a
  , zipperAfter   :: [a]
  } deriving (Eq, Foldable, Functor, Traversable)

-- | Motion within a zipper.
leftward, rightward :: Zipper a -> Maybe (Zipper a)

leftward (Zipper (p : b) c a) = Just (Zipper b p (c : a))
leftward _ = Nothing

rightward (Zipper b c (n : a)) = Just (Zipper (c : b) n a)
rightward _ = Nothing

-- | Create a zipper from a list, using a default element as the focus if the
-- list is empty.
zipperFrom :: a -> [a] -> Zipper a
zipperFrom _ (x : xs) = Zipper [] x xs
zipperFrom p []       = Zipper [] p []

-- | Get the number of elements in a zipper.
zipperLength :: Zipper a -> Int
zipperLength z = length (zipperBefore z) + 1 + length (zipperAfter z)

-- | Get the current cursor offset within a zipper, starting from 0.
zipperIndex :: Zipper a -> Int
zipperIndex = length . zipperBefore

instance Comonad Zipper where
  extract = zipperCurrent
  duplicate z = let
    offset  = zipperIndex z
    offset' = zipperLength z - offset - 1
    in Zipper
      { zipperBefore  = reverse (iterateMaybeN offset  leftward  (leftward z))
      , zipperCurrent = z
      , zipperAfter   =          iterateMaybeN offset' rightward (rightward z)
      }

-- Debugging utilities.

instance (Show a) => Show (Zipper a) where
  show z = concat
    [ unwords (map show (reverse (zipperBefore z)))
    , "(", show (zipperCurrent z), ")"
    , unwords (map show (zipperAfter z))
    ]
