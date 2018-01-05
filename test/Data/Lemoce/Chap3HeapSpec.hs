module Data.Lemoce.Chap3HeapSpec (main, spec) where

import           Data.Lemoce.Chap3.Heap
import           Prelude                as P
import           Test.Hspec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "LeftistHeap a" $ do
    it "test empty heap" $ do
      (empty :: LeftistHeap Int) `shouldBe` (E :: LeftistHeap Int)

    it "test insert" $ do
      (fromList [1..5] :: LeftistHeap Int) `shouldBe`
        (T 2 1 (T 2 3 (T 1 4 E E) (T 1 5 E E)) (T 1 2 E E))

  describe "WeightBiasedLeftistHeap a" $ do
    it "test empty heap" $ do
      (empty :: WeightBiasedLeftistHeap Int) `shouldBe` (WBLH E :: WeightBiasedLeftistHeap Int)
    it "test insert" $ do
      (fromList [1..5] :: WeightBiasedLeftistHeap Int) `shouldBe`
        (WBLH (T 5 1 (T 2 3 (T 1 4 E E) E) (T 2 2 (T 1 5 E E) E)))
