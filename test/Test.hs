import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

import qualified Turing

main :: IO ()
main =
  Hspec.hspec $ do
    Hspec.describe "Prelude.head" $ do
      Hspec.it "returns the first elem of a list" $ do
        head [23 ..] `Hspec.shouldBe` (23 :: Int)
      -- Hspec.it "returns the first element of an *arbitrary* list" $
      --   QuickCheck.property $ \x xs -> head (x : xs) == (x :: Int)
