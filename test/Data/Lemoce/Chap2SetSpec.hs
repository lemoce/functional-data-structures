module Data.Lemoce.Chap2SetSpec (main, spec) where

import           Data.Lemoce.Chap2.Set
import           Prelude as P
import           Test.Hspec

instance (Show a) => Show (UnbalancedSet a) where
  show E          = "Empty"
  show s = showHelper s 0
    where showHelper :: (Show a) => UnbalancedSet a -> Int -> String
          showHelper E n = ""
          showHelper (T l x r) n = 
            (take (2*n) $ repeat ' ') P.++ (show x) P.++ "\n" P.++
            (showHelper l (n+1)) P.++ 
            (showHelper r (n+1))

instance (Eq a) => Eq (UnbalancedSet a) where
  (==) E E = True
  (==) (T _ _ _) E = False
  (==) E (T _ _ _) = False
  (==) (T l1 x1 r1) (T l2 x2 r2) = if x1 == x1
                                       then (l1 == l2) && (r1 == r2)
                                       else False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Set a" $ do
    it "test empty set" $ do
      (empty :: UnbalancedSet Int) `shouldBe` (E :: UnbalancedSet Int)

