{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Lemoce.Chap3.Heap
  ( Heap (..)
  , LeftistHeap (..))
where

import           Control.Exception
import           Data.List         (dropWhile, head)

data Empty = Empty deriving (Show, Eq)

instance Exception Empty

class (Ord a) => Heap t a where
  empty :: t a
  isEmpty :: t a -> Bool
  insert :: a -> t a -> t a
  merge :: t a -> t a -> t a
  findMin :: t a -> a
  deleteMin :: t a -> t a

  singleton :: a -> t a
  singleton x = insert x empty


-- Used to call Height-Biased Leftist Tree
-- http://web.onda.com.br/abveiga/capitulo5-ingles.pdf
data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a) deriving Show

instance (Ord a) => Heap LeftistHeap a where
  empty = E
  isEmpty E = True
  isEmpty _ = False

  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
       if x <= y
          then makeT x a1 (merge b1 h2)
          else makeT y a2 (merge b2 h1)
    where
      rank E           = 0
      rank (T r _ _ _) = r

      makeT x ha hb = if rank ha >= rank hb
                        then T (rank hb + 1) x ha hb
                        else T (rank ha + 1) x hb ha


  insert x h = merge (T 1 x E E) h

  findMin E           = throw Empty
  findMin (T _ x _ _) = x

  deleteMin E           = throw Empty
  deleteMin (T _ x a b) = merge a b


fromList :: (Ord a) => [a] -> LeftistHeap a
fromList xs = head . head . dropWhile ((/= 1) . length) $ iterate func $ map singleton xs
  where
    func :: (Ord a) => [LeftistHeap a] -> [LeftistHeap a]
    func []       = []
    func (x: [])  = [x]
    func (x:y:xs) = merge x y : func xs

