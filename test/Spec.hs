import Control.Applicative
import Control.Monad
import Test.HUnit
import Test.Hspec
import Toody

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parsing boxes" $ do

    let
      -- Parse a box clockwise from the top left corner and return its size.
      box = do
        width   <- moving eastward  horizontal
        height  <- moving southward vertical
        width'  <- moving westward  horizontal
        guard (width == width')
        height' <- moving northward vertical
        guard (height == height')
        pure (width, height)
      horizontal = edge (equal '-')
      vertical = edge (equal '|')
      edge = fmap length . capped . many
      cap = equal '+'
      capped = between cap (lookahead cap)

    it "parses a single box" $ do

      let
        testGrid = makeGrid ' '
          [ "+----+"
          , "|    |"
          , "|    |"
          , "+----+"
          ]

      case runParser box eastward (Just testGrid) of
        Right ((4, 2), _) -> pure ()
        result -> assertFailure
          (concat
            [ "expected 'Right ((4, 2), _)' but got '"
            , show result
            , "'"
            ])
