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
  insert e t@(T l y r) = fromMaybe t (insertHelper e y t)
    where insertHelper e x E  = if e == x
                                    then Nothing
                                    else Just (T E e E)
          insertHelper e x (T l v r)
            | e < v    = fmap (\l' -> T l' v r) (insertHelper e x l)
            | otherwise = fmap (\r' -> T l v r') (insertHelper e v r)

  member e E = False
  member e t@(T l y r) = memberHelper e y t
    where memberHelper e x E = e == x
          memberHelper e x (T l y r) =
            if e < y
              then memberHelper e x l
              else memberHelper e y r

