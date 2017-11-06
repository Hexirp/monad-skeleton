{-# LANGUAGE PolyKinds, GADTs, Rank2Types, ScopedTypeVariables, Safe #-}
module Control.Category.Spider (Spider(..), transSpider, (|>), viewL, transKleisli) where
import Control.Arrow

data Spider k a b where
  NilS :: Spider k a a
  ConS :: k b c -> Spider k a b -> Spider k a c

transSpider :: forall j k a b. (forall x y. j x y -> k x y) -> Spider j a b -> Spider k a b
transSpider f = go where
  go :: forall a' b'. Spider j a' b' -> Spider k a' b'
  go NilS = NilS
  go (ConS a b) = ConS (f a) (go b)
{-# INLINE transSpider #-}

(|>) :: k b c -> Spider k a b -> Spider k a c
(|>) = ConS
{-# INLINE (|>) #-}

viewL :: forall r k x a
  .  r x
  -> (forall b c. k b c -> r b -> r c)
  -> Spider k x a
  -> r a
viewL e r = go where
  go :: forall a'. Spider k x a' -> r a'
  go NilS = e
  go (ConS x y) = r x (go y)
{-# INLINE viewL #-}

transKleisli :: (m b -> n b) -> Kleisli m a b -> Kleisli n a b
transKleisli f (Kleisli k) = Kleisli (f . k)
{-# INLINE transKleisli #-}
