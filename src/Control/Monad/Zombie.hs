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
 Zombie :: MonadView t (Zombie t) x -> Cat (Kleisli (Zombie t)) x a -> Zombie t a -> Zombie t a

instance Functor (Zombie t) where
  fmap = liftM

instance Applicative (Zombie t) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Alternative (Zombie t) where
  empty = Sunlight
  Sunlight <|> y = y
  Zombie v c xs <|> y = Zombie v c (xs <|> y)

instance Monad (Zombie t) where
  return a = Zombie (Return a) id Sunlight
  Sunlight >>= k = Sunlight
  Zombie v c xs >>= k = Zombie v (Tree c $ Leaf $ Kleisli k) (xs >>= k)

instance MonadPlus (Zombie t) where
  mzero = empty
  mplus = (<|>)

-- | Lift a unit action
liftZ :: t a -> Zombie t a
liftZ t = embalm (t :>>= return)
{-# INLINE liftZ #-}

-- | Turn a decomposed form into a composed form.
embalm :: MonadView t (Zombie t) a -> Zombie t a
embalm t = Zombie (Spine t id) Sunlight
{-# INLINE embalm #-}

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm Sunlight = []
disembalm (Zombie x xs) = disembalm' x ++ disembalm xs

disembalm' :: Spine t (Zombie t) a -> [MonadView t (Zombie t) a]
disembalm' (Spine v c) = case v of
  Return a -> viewL c [Return a] $ \(Kleisli k) c' -> case k a of
    s -> disembalm $ mapZ (graftSpine c') s
  t :>>= k -> return $ t :>>= \a -> case k a of
    s -> mapZ (graftSpine c) s

mapZ :: (Spine t (Zombie t) a -> Spine t (Zombie t) b)
     -> Zombie t a -> Zombie t b
mapZ f Sunlight = Sunlight
mapZ f (Zombie x xs) = Zombie (f x) (mapZ f xs)

-- | Like 'hoistSkeleton'
hoistZombie :: forall s t a. (forall x. s x -> t x) -> Zombie s a -> Zombie t a
hoistZombie f = go where
  go :: forall x. Zombie s x -> Zombie t x
  go Sunlight = Sunlight
  go (Zombie (Spine v c) xs) = Zombie (Spine (hoistMV f go v) (transCat (transKleisli go) c)) (go xs)
{-# INLINE hoistZombie #-}
