{-|
Module      : Toody.Parser
Description : General 2D parsing primitives.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

{-# LANGUAGE InstanceSigs #-}

module Toody.Parser
  ( ParseError
  , Parser(..)
  , between
  , boundary
  , equal
  , failure
  , getGrid
  , getLocation
  , lookahead
  , moving
  , negative
  , satisfy
  , setGrid
  , step
  ) where

import Control.Applicative (Alternative(..))
import Control.Arrow ((&&&))
import Control.Comonad (Comonad(..))
import Control.Monad (MonadPlus(..), void)
import Data.Monoid ((<>))
import Toody.Grid
import Toody.Point

-- | A parser is a function that accepts the current state of a 'Grid', and
-- either fails with a 'ParseError', or returns a parsed value and an updated
-- 'Grid', or 'Nothing' if the grid couldn't be updated.
newtype Parser c a = Parser
  { runParser :: Move c -> Maybe (Grid c) -> Either ParseError (a, Maybe (Grid c)) }

-- | A parse error message.
type ParseError = String

-- | Locally override the direction of a parser.
moving :: Move c -> Parser c a -> Parser c a
moving move (Parser p) = Parser $ \ _move grid -> p move grid

-- | Accept a cell matching a predicate and advance in the current direction.
satisfy :: (Show c) => (c -> Bool) -> Parser c c
satisfy predicate = Parser $ \ move mGrid -> do
  let mCell = extract <$> mGrid
  case mCell of
    Nothing -> Left "Toody.satisfy: unexpected grid boundary"
    Just cell
      | predicate cell -> Right (cell, move =<< mGrid)
      | otherwise      -> Left ("Toody.satisfy: failed to satisfy: " ++ show mGrid)

-- | Step one cell in a direction.
step :: (Show c) => Move c -> Parser c ()
step move = void (moving move anything)

-- | Accept a single cell equal to a given cell.
equal :: (Eq c, Show c) => c -> Parser c c
equal = satisfy . (==)

-- | Accept anything and advance in the current direction.
anything :: (Show c) => Parser c c
anything = satisfy (const True)

-- | Accepts only a grid boundary.
boundary :: Parser c ()
boundary = Parser $ \ _move mGrid -> case mGrid of
  Nothing -> Right ((), mGrid)
  Just{}  -> Left "Toody.boundary: expected grid boundary"

-- | Wraps a parser in beginning and ending delimiters.
between :: Parser c b -> Parser c e -> Parser c i -> Parser c i
between begin end item = begin *> item <* end

-- | Runs a parser, ignoring any motion it makes.
lookahead :: Parser c a -> Parser c a
lookahead (Parser p) = Parser $ \ move mGrid -> do
  (c, _mGrid') <- p move mGrid
  pure (c, mGrid)

-- TODO: Add 'try'? Would require distinguishing "failed but consumed no input"
-- from "failed and consumed input" as Parsec does.

-- | Succeeds when the supplied parser fails, and fails if it succeeds.
negative :: Parser c a -> Parser c ()
negative (Parser p) = Parser $ \ move mGrid -> case p move mGrid of
  Left{} -> Right ((), mGrid)
  Right{} -> Left "Toody.negative: expected parser failure"

instance Functor (Parser c) where
  fmap f (Parser p) = Parser $ \ move grid -> case p move grid of
    Left  e           -> Left e
    Right (c, mGrid') -> Right (f c, mGrid')

instance Applicative (Parser c) where

  pure :: a -> Parser c a
  pure c = Parser $ \ _move mGrid -> Right (c, mGrid)

  (<*>) :: Parser c (a -> b) -> Parser c a -> Parser c b
  Parser f <*> Parser x = Parser $ \ move mGrid -> do
    (f', mGrid') <- f move mGrid
    (x', mGrid'') <- x move mGrid'
    pure (f' x', mGrid'')

instance Monad (Parser c) where
  return = pure
  Parser p1 >>= f = Parser $ \ move mGrid -> do
    (c, mGrid') <- p1 move mGrid
    runParser (f c) move mGrid'
  fail message = Parser $ \ _move mGrid -> Left
    (maybe "out of bounds" (humanLocation . gridLocation) mGrid
      <> ": " <> message)

instance Alternative (Parser c) where
  empty = fail "empty"
  Parser p1 <|> Parser p2 = Parser $ \ move grid -> case p1 move grid of
    Left{}  -> p2 move grid
    success -> success

instance MonadPlus (Parser c) where
  mzero = empty
  mplus = (<|>)

-- | A parser that always fails.
failure :: Parser c a
failure = Parser $ \ _move mGrid -> Left
  (maybe "out of bounds" (humanLocation . gridLocation) mGrid
    <> ": generic parser failure")

-- | Gets the current grid location.
getLocation :: Parser c (Maybe Point)
getLocation = Parser $ \ _move mGrid -> Right ((fmap gridLocation &&& id) mGrid)

-- | Parser that returns the whole state of the grid.
getGrid :: Parser c (Maybe (Grid c))
getGrid = Parser $ \ _move mGrid -> Right (mGrid, mGrid)

-- | Teleports a parser to a new grid, typically (but not necessarily) one that
-- was previously saved with 'getGrid'.
setGrid :: Grid c -> Parser c ()
setGrid grid = Parser $ \ _move _grid -> Right ((), Just grid)
