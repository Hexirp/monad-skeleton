{-# LANGUAGE PolyKinds, GADTs, Rank2Types, ScopedTypeVariables, Safe #-}
module Control.Category.Spider (Spider(..), transSpider, (|>), viewL, transKleisli) where
import Control.Arrow

data Spider k a b where
  Leaf :: k a b -> Spider k a b
  Tree :: Spider k a b -> Spider k b c -> Spider k a c

transSpider :: (forall x y. j x y -> k x y) -> Spider j a b -> Spider k a b
transSpider f (Tree a b) = transSpider f a `Tree` transSpider f b
transSpider f (Leaf k) = Leaf (f k)
{-# INLINE transSpider #-}

(|>) :: Spider k a b -> k b c -> Spider k a c
s |> k = Tree s (Leaf k)
{-# INLINE (|>) #-}

viewL :: forall k a b r. Spider k a b
  -> (k a b -> r)
  -> (forall x. k a x -> Spider k x b -> r)
  -> r
viewL (Leaf k) e _ = e k
viewL (Tree a b) _ r = go a b where
  go :: Spider k a x -> Spider k x b -> r
  go (Leaf k) t = r k t
  go (Tree c d) t = go c (Tree d t)

transKleisli :: (m b -> n b) -> Kleisli m a b -> Kleisli n a b
transKleisli f (Kleisli k) = Kleisli (f . k)
{-# INLINE transKleisli #-}
