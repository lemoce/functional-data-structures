{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Lemoce.Chap2.Map
  ( FiniteMap (..)
  ) where

import           Control.Exception
import           Data.Lemoce.Chap2.Set
import           Data.Maybe

-- way to wrap Type
newtype UnbalancedMap k v = M { um :: UnbalancedSet (k,v)}

class FiniteMap t k v where
  empty :: t k v
  bind :: k -> v -> t k v -> t k v
  lookup :: k -> t k v -> v

data NotFound = NotFound deriving (Show, Eq)

instance Exception NotFound

instance (Ord k) => FiniteMap UnbalancedMap k v where
  empty = M E

  bind key value m@(M t@(T _ y _)) = M (fromMaybe t (bindHelper (key, value) y t))
    where bindHelper e@(ek, ev) (xk, xv) E = if ek == xk
                                                    then Nothing
                                                    else Just (T E e E)
          bindHelper e@(ek, ev) x@(xk, xv) (T l v@(vk, vx) r)
            | ek < vk = fmap (\l' -> T l' v r) (bindHelper e x l)
            | otherwise = fmap (\r' -> T l v r') (bindHelper e v r)


  lookup _ (M E) = throw NotFound
  lookup key (M t@(T l y r)) = lookupHelper key y t
    where lookupHelper :: (Ord k) => k -> (k, v) -> UnbalancedSet (k, v) -> v
          lookupHelper e x@(xk, xv) E = if e == xk
                                            then xv
                                            else throw NotFound
          lookupHelper e (xk, xv) (T l' (yk', yv') r') =
            if e < yk'
                then lookupHelper e (xk, xv) l'
                else lookupHelper e (yk', yv') r'
