{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Lemoce.Chap2.Set 
  ( Set (..)
  , UnbalancedSet (..)  
  ) where

class Set t a where
  empty :: t a
  insert :: a -> t a -> t a
  member :: a -> t a -> Bool

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)

instance (Ord a) => Set UnbalancedSet a where
  empty = E

  insert e E = T E e E
  insert e t@(T l x r) =
    if e < x
      then T (insert e l) x r
      else if e > x
             then T l x (insert e r)
             else t

  member e E = False
  member e t@(T l y r) = memberHelper e y t
    where memberHelper e x E = e == x
          memberHelper e x (T l y r) =
            if e < y
              then memberHelper e x l
              else memberHelper e y r

