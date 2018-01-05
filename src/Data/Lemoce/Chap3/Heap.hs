{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Lemoce.Chap3.Heap
  ( Heap (..)
  , LeftistHeap (..)
  , WeightBiasedLeftistHeap (..) )

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

  fromList :: [a] -> t a
  fromList xs = head . head . dropWhile ((/= 1) . length) $ iterate func $ map singleton xs
    where
      func []       = []
      func (x: [])  = [x]
      func (x:y:xs) = merge x y : func xs



-- Used to call Height-Biased Leftist Tree
-- http://web.onda.com.br/abveiga/capitulo5-ingles.pdf
data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a) deriving (Eq, Show)

rank E           = 0
rank (T r _ _ _) = r

makeTHBLF x ha hb = if (rank ha) >= (rank hb)
                        then T (rank hb + 1) x ha hb
                        else T (rank ha + 1) x hb ha

makeTWBLF x ha hb = if rank ha >= rank hb
                       then T (rank ha + rank hb + 1) x ha hb
                       else T (rank ha + rank hb + 1) x hb ha


_merge :: (Ord a) => (a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a) ->
                     LeftistHeap a ->
                     LeftistHeap a ->
                     LeftistHeap a
_merge _ h E = h
_merge _ E h = h
_merge makeTFunc h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
  if x <= y
     then makeTFunc x a1 (_merge makeTFunc b1 h2)
     else makeTFunc y a2 (_merge makeTFunc h1 b2)


instance (Ord a) => Heap LeftistHeap a where
  empty = E
  isEmpty E = True
  isEmpty _ = False

  merge = _merge makeTHBLF

  insert x h = merge (T 1 x E E) h

  findMin E           = throw Empty
  findMin (T _ x _ _) = x

  deleteMin E           = throw Empty
  deleteMin (T _ x a b) = merge a b



newtype (Ord a) => WeightBiasedLeftistHeap a = WBLH { lh :: LeftistHeap a } deriving (Show, Eq)

instance (Ord a) => Heap WeightBiasedLeftistHeap a where
  empty = WBLH E
  isEmpty (WBLH E) = True
  isEmpty _        = False

  insert x h = merge (WBLH (T 1 x E E)) h

  merge a b = WBLH $ _merge makeTWBLF (lh a) (lh b)

  findMin (WBLH E)           = throw Empty
  findMin (WBLH (T _ x _ _)) = x
  deleteMin (WBLH E)           = throw Empty
  deleteMin (WBLH (T _ x a b)) = merge (WBLH a) (WBLH b)

