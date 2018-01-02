{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Lemoce.Chap2.Set
  ( Set (..)
  , UnbalancedSet (..)
  ) where

import           Data.Maybe (fromMaybe)

class Set t a where
  empty :: t a
  insert :: a -> t a -> t a
  member :: a -> t a -> Bool

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)

instance (Ord a) => Set UnbalancedSet a where
  empty = E

  -- Grabbed the code from https://github.com/qnikst/okasaki/blob/master/ch02/ex_2_3.hs. Thanks for sharing.
  insert e E = T E e E
  insert e t@(T l x r) = fromMaybe t (insertHelper e t)
    where insertHelper e E = Just (T E e E)
          insertHelper e (T l x' r)
            | e == x' = Nothing
            | e < x' = fmap (\l' -> T l' x' r) (insertHelper e l)
            | e > x' = fmap (\r' -> T l x' r') (insertHelper e r)

  member e E = False
  member e t@(T l y r) = memberHelper e y t
    where memberHelper e x E = e == x
          memberHelper e x (T l y r) =
            if e < y
              then memberHelper e x l
              else memberHelper e y r

