{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Lemoce.Chap2.Stack
  ( Stack(..)
  , List(..)
  , Empty(..)
  , Subscript(..)
  , (++)
  , update
  , suffixes
  ) where

import           Control.Exception
import           Prelude           hiding (head, tail, (++))

instance Exception Empty
instance Exception Subscript

class Stack t a where
  empty :: t a
  isEmpty :: t a -> Bool
  cons :: a -> t a -> t a
  head :: t a -> a
  tail :: t a -> t a

(++) :: (Stack t a) => t a -> t a -> t a
(++) lst1 lst2 = if isEmpty lst1
                    then lst2
                    else cons (head lst1) ((tail lst1) ++ lst2)

update :: (Stack t a, Eq n, Num n) => t a -> n -> a -> t a
update stack i x
  | isEmpty stack && i /= 0 = throw Subscript
  | i == 0 = cons x (tail stack)
  | otherwise = cons (head stack) (update (tail stack) (i-1) x)

data List a = Nil | Cons a (List a)

data Empty = Empty deriving (Show, Eq)
data Subscript = Subscript deriving (Show, Eq)

instance Stack List a where
  empty              = Nil
  isEmpty Nil = True
  isEmpty _   = False
  cons a lst         = Cons a lst
  head Nil          = throw Empty
  head (Cons a lst) = a
  tail Nil          = throw Empty
  tail (Cons a lst) = lst


suffixes :: List a -> List (List a)
suffixes Nil = Cons Nil Nil
suffixes lst = Cons lst $ suffixes (tail lst)

