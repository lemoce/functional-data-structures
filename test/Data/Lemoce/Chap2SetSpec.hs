module Data.Lemoce.Chap2SetSpec (main, spec) where

import           Data.Lemoce.Chap2.Set
import           Prelude               as P
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

instance (Show a) => Show (Tree a) where
  show Leaf       = "Empty"
  show s = showHelper s 0
    where showHelper :: (Show a) => Tree a -> Int -> String
          showHelper Leaf n = ""
          showHelper (Branch l x r) n =
            (take (2*n) $ repeat ' ') P.++ (show x) P.++ "\n" P.++
            (showHelper l (n+1)) P.++
            (showHelper r (n+1))

instance (Eq a) => Eq (Tree a) where
  (==) Leaf Leaf = True
  (==) (Branch _ _ _) Leaf = False
  (==) Leaf (Branch _ _ _) = False
  (==) (Branch l1 x1 r1) (Branch l2 x2 r2) = if x1 == x1
                                                  then (l1 == l2) && (r1 == r2)
                                                  else False

main :: IO ()
main = do
  hspec spec
  print $ show $ makeBalancedTree 'a' 5

spec :: Spec
spec = do
  describe "Set a" $ do
    it "test empty set" $ do
      (empty :: UnbalancedSet Int) `shouldBe` (E :: UnbalancedSet Int)

    it "insert 20 on set" $ do
      insert 20 (T (T (T E 1 E) 4 (T E 6 E)) 8 (T (T E 9 E) 30 (T E 31 E))) `shouldBe`
        (T (T (T E 1 E) 4 (T E 6 E)) 8 (T (T E 9 (T E 20 E)) 30 (T E 31 E)))

    it "insert 10 on set" $ do
      insert 10 (T (T (T E 1 E) 4 (T E 6 E)) 8 (T (T E 9 E) 30 (T E 31 E))) `shouldBe`
        (T (T (T E 1 E) 4 (T E 6 E)) 8 (T (T E 9 (T E 10 E)) 30 (T E 31 E)))

    it "insert repeat element" $ do
      insert 31 (T (T (T E 1 E) 4 (T E 6 E)) 8 (T (T E 9 E) 30 (T E 31 E))) `shouldBe`
        (T (T (T E 1 E) 4 (T E 6 E)) 8 (T (T E 9 E) 30 (T E 31 E)))

    it "test complete binary tree" $ do
      ((makeCompleteTree 1 4) :: Tree Int) `shouldBe`
        ((Branch (Branch (Branch (Branch Leaf 1 Leaf) 1 (Branch Leaf 1 Leaf)) 1 (Branch (Branch Leaf 1 Leaf) 1 (Branch Leaf 1 Leaf))) 1 (Branch (Branch (Branch Leaf 1 Leaf) 1 (Branch Leaf 1 Leaf)) 1 (Branch (Branch Leaf 1 Leaf) 1 (Branch Leaf 1 Leaf)))) :: Tree Int)

