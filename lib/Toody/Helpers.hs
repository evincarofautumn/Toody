{-|
Module      : Toody.Helpers
Description : Higher-level parsing helpers.
Copyright   : (c) Jon Purdy, 2017
License     : MIT
Maintainer  : evincarofautumn@gmail.com
-}

{-# LANGUAGE DeriveFunctor #-}

module Toody.Helpers
  ( BoxStyle(..)
  , asciiBox
  , asciiBoxStyle
  , box
  , doubleBox
  , doubleBoxStyle
  , lightBox
  , lightBoxStyle
  ) where

import Control.Applicative (many)
import Control.Monad (guard)
import Toody.Box (Box(..), Size(..))
import Toody.Parser
import qualified Toody.Grid as Grid

-- | The style of a basic box described by the cells of its corners and edges.
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

  width   <- moving Grid.eastward  (between topLeft     (lookahead topRight)    horizontal)
  height  <- moving Grid.southward (between topRight    (lookahead bottomRight) vertical)
  width'  <- moving Grid.westward  (between bottomRight (lookahead bottomLeft)  horizontal)
  height' <- moving Grid.northward (between bottomLeft  (lookahead topLeft)     vertical)

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
