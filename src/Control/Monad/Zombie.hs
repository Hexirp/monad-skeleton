{-# LANGUAGE Rank2Types, ScopedTypeVariables, GADTs #-}
module Control.Monad.Zombie where
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Skeleton
import Control.Monad.Skeleton.Internal (transKleisli)
import Control.Monad.Zombie.Internal
import Prelude hiding (id, (.))

-- | The spine of skeleta.
data Spine t m a where
  Spine :: MonadView t m a -> Cat (Kleisli m) a b -> Spine t m b

-- | Extend a spine.
graftSpine :: Cat (Kleisli m) a b -> Spine t m a -> Spine t m b
graftSpine c (Spine v d) = Spine v (Tree d c)
{-# INLINE graftSpine #-}

-- | 'Zombie' is a variant of 'Skeleton' which has an 'Alternative' instance.
data Zombie t a where
 Sunlight :: Zombie t a
 ReturnZ :: x -> Cat (Kleisli (Zombie t)) x a -> Zombie t a -> Zombie t a
 BindZ :: t y -> (y -> Zombie t x) -> Cat (Kleisli (Zombie t)) x a -> Zombie t a -> Zombie t a

instance Functor (Zombie t) where
  fmap = liftM

instance Applicative (Zombie t) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Alternative (Zombie t) where
  empty = Sunlight
  Sunlight <|> y = y
  ReturnZ x c xs <|> y = ReturnZ x c (xs <|> y)
  BindZ y z c xs <|> y = BindZ y z c (xs <|> y)

instance Monad (Zombie t) where
  return a = Zombie (Return a) id Sunlight
  Sunlight >>= k = Sunlight
  ReturnZ x c xs >>= k = Zombie x (Tree c $ Leaf $ Kleisli k) (xs >>= k)
  BindZ y z c xs >>= k = Zombie y z (Tree c $ Leaf $ Kleisli k) (xs >>= k)

instance MonadPlus (Zombie t) where
  mzero = empty
  mplus = (<|>)

-- | Lift a unit action
liftZ :: t a -> Zombie t a
liftZ t = embalm (t :>>= return)
{-# INLINE liftZ #-}

-- | Turn a decomposed form into a composed form.
embalm :: MonadView t (Zombie t) a -> Zombie t a
embalm (Return x) = ReturnZ x id Sunlight
embalm (y :>>= z) = BindZ y z id Sunlight
{-# INLINE embalm #-}

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm Sunlight = []
disembalm (Zombie v c xs) = disembalm' v c ++ disembalm xs

disembalm' :: MonadView t (Zombie t) x -> Cat (Kleisli (Zombie t)) x a -> [MonadView t (Zombie t) a]
disembalm' v c = case v of
  Return a -> viewL c [Return a] $ \(Kleisli k) c' -> case k a of
    s -> disembalm $ mapZ c' s
  t :>>= k -> return $ t :>>= \a -> case k a of
    s -> mapZ c s

mapZ :: Cat (Kleisli (Zombie t)) a b -> Zombie t a -> Zombie t b
mapZ f Sunlight = Sunlight
mapZ f (Zombie v c xs) = Zombie v (Tree c f) (mapZ f xs)

-- | Like 'hoistSkeleton'
hoistZombie :: forall s t a. (forall x. s x -> t x) -> Zombie s a -> Zombie t a
hoistZombie f = go where
  go :: forall x. Zombie s x -> Zombie t x
  go Sunlight = Sunlight
  go (Zombie v c xs) = Zombie (hoistMV f go v) (transCat (transKleisli go) c) (go xs)
{-# INLINE hoistZombie #-}
