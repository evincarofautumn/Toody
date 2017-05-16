import Data.Char (isDigit, isLetter, isSpace)
import Control.Applicative
import Control.Monad
import Data.Foldable (asum)
import Data.List (foldl')
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

    it "finds nearest box in a direction" $ do

      let
        testGrid = makeGrid ' '
          [ "     ┌───┐"
          , "     │   │"
          , "┌───┐└───┘"
          , "│   │     "
          , "└───┘     "
          ]
        box' = nearest lightBox

      (fst <$> runParser box' southward (Just testGrid))
        `shouldBe` Right (Box (Point 2 0) (Size 3 1))

      (fst <$> runParser box' eastward (Just testGrid))
        `shouldBe` Right (Box (Point 0 5) (Size 3 1))

    it "parses math expressions containing matrices" $ do

      let

        exp = eqExp

        eqExp = do
          a <- addExp
          lexeme (equal '=')
          b <- addExp
          pure (Equ1 a b)

        addExp = do
          a <- mulExp
          as <- many addSuffix
          pure (if null as then a else foldl' Add1 a as)

        addSuffix = lexeme (equal '+') *> mulExp

        mulExp = do
          a <- term
          as <- many mulSuffix
          pure (if null as then a else foldl' Mul1 a as)

        mulSuffix = lexeme (equal '*') *> term

        term = asum [par, lit, var, mat]

        lit = Lit1 . read <$> lexeme (some (satisfy isDigit))

        var = Var1 <$> lexeme (some (satisfy isLetter))

        -- Equations aren't allowed within expressions, so we use addExp, not exp.
        par = between (lexeme (equal '(')) (lexeme (equal ')')) addExp

        -- Parse a matrix at the current location by finding the height of its
        -- left bracket, collecting the corresponding number of rows of terms,
        -- then finding its right bracket and ensuring it matches the left.
        mat = lexeme $ do
          loc1 <- getLocation
          leftHeight <- length <$> lookahead (moving southward (some (equal '[')))
          step eastward <* spaces
          loc2 <- getLocation
          rows <- lookahead (replicateM leftHeight
            (lookahead ((,) <$> many term <*> getGrid) <* step southward))
          let mEnd = snd (head rows)
          case mEnd of
            Nothing -> fail "expected end of matrix but got out of bounds"
            -- TODO: This pattern of saving & restoring location could have a
            -- cleaner API.
            Just grid -> setGrid grid
          spaces
          rightHeight <- length <$> lookahead (moving southward (some (equal ']')))
          guard (leftHeight == rightHeight)
          step eastward
          pure (Mat1 (map fst rows))

        lexeme = (<* spaces)
        spaces = many (satisfy isSpace)

        [va, vb, vc, vd, ve, vf, vg, vh] = Var1 . (:[]) <$> "abcdefgh"

      let
        testGrid = makeGrid ' '
          [ "a = a"
          ]
        in (fst <$> runParser exp eastward (Just testGrid))
          `shouldBe` Right (Equ1 va va {- voom -})

      let
        testGrid = makeGrid ' '
          [ "a + b = b + a"
          ]
        in (fst <$> runParser exp eastward (Just testGrid))
          `shouldBe` Right (Equ1 (Add1 va vb) (Add1 vb va))

      let
        testGrid = makeGrid ' '
          [ "(a) = (a)"
          ]
        in (fst <$> runParser exp eastward (Just testGrid))
          `shouldBe` Right (Equ1 va va)

      let
        testGrid = makeGrid ' '
          [ "a * b + c * d = d * c + b * a"
          ]
        in (fst <$> runParser exp eastward (Just testGrid))
          `shouldBe` Right
          (Equ1
            (Add1 (Mul1 va vb) (Mul1 vc vd))
            (Add1 (Mul1 vd vc) (Mul1 vb va)))

      let
        testGrid = makeGrid ' '
          [ "[ a b ] = [ 1 0 ] "
          , "[ c d ]   [ 0 1 ] "
          ]
        in (fst <$> runParser exp eastward (Just testGrid))
          `shouldBe` Right
          (Equ1
            (Mat1 [[va, vb], [vc, vd]])
            (Mat1 [[Lit1 1, Lit1 0], [Lit1 0, Lit1 1]]))

      let
        testGrid = makeGrid ' '
          [ "[ a b ] + [ e f ] = [ 0 0 ]"
          , "[ c d ]   [ g h ]   [ 0 0 ]"
          ]
        in (fst <$> runParser exp eastward (Just testGrid))
          `shouldBe` Right
          (Equ1
            (Add1
              (Mat1 [[va, vb], [vc, vd]])
              (Mat1 [[ve, vf], [vg, vh]]))
            (Mat1 [[Lit1 0, Lit1 0], [Lit1 0, Lit1 0]]))

      let
        testGrid = makeGrid ' '
          [ "[ a b ] + [ e f ] = [ (a + e) (b + f) ]"
          , "[ c d ]   [ g h ]   [ (c + g) (d + h) ]"
          ]
        in (fst <$> runParser exp eastward (Just testGrid))
          `shouldBe` Right
          (Equ1
            (Add1
              (Mat1 [[va, vb], [vc, vd]])
              (Mat1 [[ve, vf], [vg, vh]]))
            (Mat1
              [ [Add1 va ve, Add1 vb vf]
              , [Add1 vc vg, Add1 vd vh]
              ]))

      let
        testGrid = makeGrid ' '
          [ "[ a b ] * [ e f ] = [ (a * e + b * g) (a * f + b * h) ]"
          , "[ c d ]   [ g h ]   [ (c * e + d * g) (c * f + d * h) ]"
          ]
        in (fst <$> runParser exp eastward (Just testGrid))
          `shouldBe` Right
          (Equ1
            (Mul1
              (Mat1
                [ [va, vb]
                , [vc, vd]
                ])
              (Mat1
                [ [ve, vf]
                , [vg, vh]
                ]))
            (Mat1
              [ [Add1 (Mul1 va ve) (Mul1 vb vg), Add1 (Mul1 va vf) (Mul1 vb vh)]
              , [Add1 (Mul1 vc ve) (Mul1 vd vg), Add1 (Mul1 vc vf) (Mul1 vd vh)]
              ]))

data Exp1
  = Add1 Exp1 Exp1
  | Mul1 Exp1 Exp1
  | Equ1 Exp1 Exp1
  | Lit1 Int
  | Var1 String
  | Mat1 [[Exp1]]
  deriving (Eq, Show)
