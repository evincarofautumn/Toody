{-|
Module      : Toody.Utils
Description : Internal utilities.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

module Toody.Utils
  ( iterateMaybe
  , iterateMaybeN
  ) where

-- | Accumulates the results of repeatedly applying a function until it returns
-- 'Nothing'. Returns @[]@ if the initial input is 'Nothing'.
iterateMaybe :: (a -> Maybe a) -> Maybe a -> [a]
iterateMaybe f = go
  where
    go (Just x) = x : go (f x)
    go Nothing = []

-- | 'iterateMaybeN n f' is 'iterateMaybe' limited to @n@ results.
iterateMaybeN :: Int -> (a -> Maybe a) -> Maybe a -> [a]
iterateMaybeN n f = take n . iterateMaybe f
