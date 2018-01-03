module Data.Lemoce.Chap2MapSpec (main, spec) where

import           Data.Lemoce.Chap2.Map
import qualified Data.Lemoce.Chap2.Set as S
import           Prelude               as P hiding (lookup)
import           Test.Hspec


instance (Show a) => Show (S.UnbalancedSet a) where
  show S.E          = "Empty"
  show s = showHelper s 0
    where showHelper :: (Show a) => S.UnbalancedSet a -> Int -> String
          showHelper S.E n = ""
          showHelper (S.T l x r) n =
            (take (2*n) $ repeat ' ') P.++ (show x) P.++ "\n" P.++
            (showHelper l (n+1)) P.++
            (showHelper r (n+1))

instance (Eq a) => Eq (S.UnbalancedSet a) where
  (==) S.E S.E = True
  (==) (S.T _ _ _) S.E = False
  (==) S.E (S.T _ _ _) = False
  (==) (S.T l1 x1 r1) (S.T l2 x2 r2) = if x1 == x1
                                           then (l1 == l2) && (r1 == r2)
                                           else False

instance (Show k, Show v) => Show (UnbalancedMap k v) where
  show m = show $ um m

instance (Eq k, Eq v) => Eq (UnbalancedMap k v) where
  (==) m1 m2 = (um m1) == (um m2)


main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "Map a" $ do
    it "test empty map" $ do
       (empty :: UnbalancedMap Int [Char]) `shouldBe` (M S.E :: UnbalancedMap Int [Char])

    it "test lookup" $ do
      (lookup 3 (M (S.T (S.T S.E (1, "one") S.E) (2, "two") (S.T S.E (3, "three") S.E)) :: UnbalancedMap Integer [Char])) `shouldBe`
        "three"

    it "test bind" $ do
        (bind 3 "three" $ bind 1 "one" $ bind 2 "two" $ empty :: UnbalancedMap Integer [Char])
          `shouldBe`
            (M (S.T (S.T S.E (1, "one") S.E) (2, "two") (S.T S.E (3, "three") S.E)) :: UnbalancedMap Integer [Char])
