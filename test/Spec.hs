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

    it "parses a single ASCII box" $ do

      let
        testGrid = makeGrid ' '
          [ "+----+"
          , "|    |"
          , "|    |"
          , "+----+"
          ]

      (fst <$> runParser asciiBox eastward (Just testGrid))
        `shouldBe` Right (Box (Point 0 0) (Size 4 2))

    it "parses a single Unicode box" $ do

      let
        testGrid = makeGrid ' '
          [ "┌───┐"
          , "│   │"
          , "└───┘"
          ]

      (fst <$> runParser lightBox eastward (Just testGrid))
        `shouldBe` Right (Box (Point 0 0) (Size 3 1))

    it "finds and parses multiple boxes" $ do
      let
        testGrid = makeGrid ' '
          [ "           +----+"
          , "    +---+  |    |"
          , "    |   |  +----+"
          , "    +---+        "
          ]
        box' = everywhere asciiBox

      (fst <$> runParser box' eastward (Just testGrid))
        `shouldBe` Right [Box (Point 0 11) (Size 4 1), Box (Point 1 4) (Size 3 1)]

    it "finds and parses multiple different Unicode boxes" $ do
      let
        testGrid = makeGrid ' '
          [ "           ╔════╗"
          , "    ┌───┐  ║    ║"
          , "    │   │  ╚════╝"
          , "    └───┘        "
          ]
        box' = everywhere (lightBox <|> doubleBox)

      (fst <$> runParser box' eastward (Just testGrid))
        `shouldBe` Right [Box (Point 0 11) (Size 4 1), Box (Point 1 4) (Size 3 1)]
