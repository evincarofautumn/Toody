{-|
Module      : Toody.Search
Description : Parser combinators for searching in a grid.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

module Toody.Search
  ( everywhere
  , nearest
  ) where

import Control.Comonad (Comonad(..))
import Toody.Parser (Parser(Parser))

-- | Runs a parser at every location on a grid and returns the list of
-- successful results in north-to-south, west-to-east order.
everywhere :: Parser c a -> Parser c [a]
everywhere (Parser parse) = Parser $ \ move mGrid -> case mGrid of
  Nothing -> pure ([], Nothing)
  Just grid -> let
    possibilities = duplicate grid
    collectSuccess mc acc = case mc of
      Left{} -> acc
      Right (c, _) -> c : acc
    results = foldr collectSuccess [] (fmap (parse move . Just) possibilities)
    in Right (results, mGrid)

-- | Runs a parser at each step in the current direction until it succeeds.
-- Fails if the grid boundary is reached without a successful parse.
nearest :: Parser c a -> Parser c a
nearest (Parser parse) = Parser seek
  where
    seek move = go
      where
        go mGrid = case parse move mGrid of
          Right (result, mGrid') -> Right (result, mGrid')
          Left{} -> case mGrid of
            Nothing -> Left "cannot find nearest outside grid"
            Just grid -> case move grid of
              mGrid'@Just{} -> go mGrid'
              Nothing -> Left "got to edge of grid without finding nearest"
