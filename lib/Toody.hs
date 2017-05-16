{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Toody
  ( Box(..)
  , BoxStyle(..)
  , Column
  , Grid(..)
  , Height
  , Move
  , Neighborhood(..)
  , ParseError
  , Parser(runParser)
  , Point(..)
  , Row
  , Size(..)
  , Width
  , Zipper(..)
  , asciiBox
  , asciiBoxStyle
  , between
  , box
  , doubleBox
  , doubleBoxStyle
  , eastward
  , equal
  , everywhere
  , getGrid
  , getLocation
  , gridLocation
  , leftward
  , lightBox
  , lightBoxStyle
  , lookahead
  , makeGrid
  , makeZipper
  , moving
  , nearest
  , neighborhood
  , northeastward
  , northward
  , northwestward
  , rightward
  , satisfy
  , setGrid
  , southeastward
  , southward
  , southwestward
  , step
  , westward
  , zipperIndex
  , zipperLength
  ) where

import Control.Applicative (Alternative(..))
import Control.Arrow ((&&&))
import Control.Comonad (Comonad(..))
import Control.Monad (MonadPlus(..), (<=<), guard, void)
import Data.Foldable (Foldable(..))
import Data.Function (on)
import Data.List (intercalate)
import Data.Monoid ((<>))

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
  deriving (Eq, Foldable, Functor, Traversable)

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

gridLocation :: Grid a -> Point
gridLocation = uncurry Point . (zipperIndex &&& zipperIndex . zipperCurrent) . unGrid

-- Parsing primitives.

-- | A parser is a function that accepts the current state of a 'Grid', and
-- either fails with a 'ParseError', or returns a parsed value and an updated
-- 'Grid', or 'Nothing' if the grid couldn't be updated.
newtype Parser c a = Parser
  { runParser :: Move c -> Maybe (Grid c) -> Either ParseError (a, Maybe (Grid c)) }

type ParseError = String

type Move c = Grid c -> Maybe (Grid c)

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

-- | Accept only a grid boundary.
boundary :: Parser c ()
boundary = Parser $ \ _move mGrid -> case mGrid of
  Nothing -> Right ((), mGrid)
  Just c  -> Left "Toody.boundary: expected grid boundary"

between :: Parser c b -> Parser c e -> Parser c i -> Parser c i
between begin end item = begin *> item <* end

-- | Run a parser, ignoring any motion it makes.
lookahead :: Parser c a -> Parser c a
lookahead (Parser p) = Parser $ \ move mGrid -> do
  (c, _mGrid') <- p move mGrid
  pure (c, mGrid)

-- TODO: Add 'try'? Would require distinguishing "failed but consumed no input"
-- from "failed and consumed input" as Parsec does.

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
    Left e  -> p2 move grid
    success -> success

instance MonadPlus (Parser c) where
  mzero = empty
  mplus = (<|>)

failure :: Parser c a
failure = Parser $ \ _move mGrid -> Left
  (maybe "out of bounds" (humanLocation . gridLocation) mGrid
    <> ": generic parser failure")

humanLocation :: Point -> String
humanLocation (Point row column) = show row <> ":" <> show column

getLocation :: Parser c (Maybe Point)
getLocation = Parser $ \ _move mGrid -> Right ((fmap gridLocation &&& id) mGrid)

getGrid :: Parser c (Maybe (Grid c))
getGrid = Parser $ \ _move mGrid -> Right (mGrid, mGrid)

setGrid :: Grid c -> Parser c ()
setGrid grid = Parser $ \ _move _grid -> Right ((), Just grid)

-- Parser searching combinators.

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

-- Box parser utilities.

data BoxStyle c = BoxStyle
  { boxTopLeft, boxTopRight, boxBottomLeft, boxBottomRight
  , boxHorizontal, boxVertical :: c
  } deriving (Eq, Functor, Show)

-- | Predefined box styles.
asciiBoxStyle, lightBoxStyle, doubleBoxStyle :: BoxStyle Char

asciiBoxStyle = BoxStyle
  { boxTopLeft     = '+'
  , boxTopRight    = '+'
  , boxBottomLeft  = '+'
  , boxBottomRight = '+'
  , boxHorizontal  = '-'
  , boxVertical    = '|'
  }

lightBoxStyle = BoxStyle
  { boxTopLeft     = '┌'
  , boxTopRight    = '┐'
  , boxBottomLeft  = '└'
  , boxBottomRight = '┘'
  , boxHorizontal  = '─'
  , boxVertical    = '│'
  }

doubleBoxStyle = BoxStyle
  { boxTopLeft     = '╔'
  , boxTopRight    = '╗'
  , boxBottomLeft  = '╚'
  , boxBottomRight = '╝'
  , boxHorizontal  = '═'
  , boxVertical    = '║'
  }

-- | Convenience parsers for different box styles.
asciiBox, lightBox, doubleBox :: Parser Char Box

asciiBox  = box asciiBoxStyle
lightBox  = box lightBoxStyle
doubleBox = box doubleBoxStyle

-- | Parse a box clockwise from the top left corner according to the given style
-- and return its location and size.
box :: (Eq c, Show c) => BoxStyle c -> Parser c Box
box style = do
  location <- maybe (fail "cannot parse box outside grid") pure =<< getLocation

  width   <- moving eastward  (between topLeft     (lookahead topRight)    horizontal)
  height  <- moving southward (between topRight    (lookahead bottomRight) vertical)
  width'  <- moving westward  (between bottomRight (lookahead bottomLeft)  horizontal)
  height' <- moving northward (between bottomLeft  (lookahead topLeft)     vertical)

  guard (width == width' && height == height')
  pure (Box location (Size width height))
  where

    topLeft     = equal (boxTopLeft style)
    topRight    = equal (boxTopRight style)
    bottomLeft  = equal (boxBottomLeft style)
    bottomRight = equal (boxBottomRight style)

    horizontal  = edge (equal (boxHorizontal style))
    vertical    = edge (equal (boxVertical style))

    edge        = fmap length . many

data Point = Point !Row !Column
  deriving (Eq, Show)

type Row = Int

type Column = Int

type Width = Int

type Height = Int

data Size = Size !Width !Height
  deriving (Eq, Show)

data Box = Box !Point !Size
  deriving (Eq, Show)

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
