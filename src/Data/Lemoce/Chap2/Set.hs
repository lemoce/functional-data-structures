{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Lemoce.Chap2.Set
  ( Set (..)
  , UnbalancedSet (..)
  , Tree (..)
  , makeCompleteTree
  , makeBalancedTree
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


data Tree a = Leaf | Branch (Tree a) a (Tree a)

makeCompleteTree :: (Ord a) => a -> Int -> Tree a
makeCompleteTree elem depth = makeHelper elem depth Leaf
  where makeHelper e n t
          | n == 0 = t
          | otherwise = makeHelper e (n - 1) (Branch t e t)


makeBalancedTree :: (Ord a) => a -> Int -> Tree a
makeBalancedTree elem 0 = Leaf
makeBalancedTree elem size =
  let leftsize = (size - 1) `div` 2
      rightsize = (size - 1 - leftsize)
      leftnode = makeBalancedTree elem (size - 1)
      rightnode = if leftsize == rightsize
                     then leftnode
                     else makeBalancedTree elem (rightsize)
  in Branch leftnode elem rightnode
