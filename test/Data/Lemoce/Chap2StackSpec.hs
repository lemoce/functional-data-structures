module Data.Lemoce.Chap2StackSpec (main, spec) where

import           Data.Lemoce.Chap2.Stack
import           Prelude           hiding (head)
import           Test.Hspec

instance (Show a) => Show (List a) where
  show Nil          = "Nil"
  show (Cons a lst) = "Cons " Prelude.++ (show a) Prelude.++ " " Prelude.++ (show lst)

instance (Eq a) => Eq (List a) where
  (==) Nil Nil = True
  (==) (Cons a lst) Nil = False
  (==) Nil (Cons a lst) = False
  (==) (Cons a lst1) (Cons b lst2) = if a == b
                                        then lst1 == lst2
                                        else False
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "List a" $ do
    it "test empty list" $ do
      (empty :: List Int) `shouldBe` (Nil :: List Int)

    it "test head empty" $ do
      head Nil `shouldThrow` (== Empty)


    it "test (++)" $ do
      (Cons 1 (Cons 2 (Cons 3 Nil))) Data.Lemoce.Chap2.Stack.++ (Cons 4 (Cons 5 (Cons 6 Nil))) `shouldBe`
        (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 Nil))))))

    it "test update" $ do
      (update (Cons 1 (Cons 2 (Cons 3 Nil))) 2 10) `shouldBe` (Cons 1 (Cons 2 (Cons 10 Nil)))

    it "test suffixes" $ do
      suffixes (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) `shouldBe`
        (Cons 
          (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
          (Cons 
            (Cons 2 (Cons 3 (Cons 4 Nil)))
            (Cons 
              (Cons 3 (Cons 4 Nil))
              (Cons 
                (Cons 4 Nil)
                (Cons Nil Nil)))))
