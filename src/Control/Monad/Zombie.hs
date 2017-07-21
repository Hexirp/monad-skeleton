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
 ReturnZ :: a -> Zombie t a -> Zombie t a
 BindZ :: t y -> Cat (Kleisli (Zombie t)) y a -> Zombie t a -> Zombie t a

instance Functor (Zombie t) where
  fmap = liftM

instance Applicative (Zombie t) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Alternative (Zombie t) where
  empty = Sunlight
  Sunlight <|> ys = ys
  ReturnZ x xs <|> ys = ReturnZ x (xs <|> ys)
  BindZ x c xs <|> ys = BindZ x c (xs <|> ys)

instance Monad (Zombie t) where
  return a = ReturnZ a Sunlight
  Sunlight >>= k = Sunlight
  ReturnZ x xs >>= k = (k x) <|> (xs >>= k)
  BindZ x c xs >>= k = BindZ x (Tree c $ Leaf $ Kleisli k) (xs >>= k)

instance MonadPlus (Zombie t) where
  mzero = empty
  mplus = (<|>)

-- | Lift a unit action
liftZ :: t a -> Zombie t a
liftZ t = embalm (t :>>= return)
{-# INLINE liftZ #-}

-- | Turn a decomposed form into a composed form.
embalm :: MonadView t (Zombie t) a -> Zombie t a
embalm (Return x) = ReturnZ x Sunlight
embalm (y :>>= z) = BindZ y (Leaf $ Kleisli z) Sunlight
{-# INLINE embalm #-}

-- | Decompose a zombie as a list of possibilities.
disembalm :: Zombie t a -> [MonadView t (Zombie t) a]
disembalm Sunlight = []
disembalm (ReturnZ x xs) = disembalmR x ++ disembalm xs
disembalm (BindZ x c xs) = disembalmB x c ++ disembalm xs

disembalmR :: a -> [MonadView t (Zombie t) a]
disembalmR a = [Return a]

disembalmB :: t y -> Cat (Kleisli (Zombie t)) y a -> [MonadView t (Zombie t) a]
disembalmB = undefined

-- | Like 'hoistSkeleton'
hoistZombie :: forall s t a. (forall x. s x -> t x) -> Zombie s a -> Zombie t a
hoistZombie f = go where
  go :: forall x. Zombie s x -> Zombie t x
  go Sunlight = Sunlight
  go (ReturnZ x xs) = ReturnZ x (go xs)
  go (BindZ x c xs) = BindZ (f x) (transCat (transKleisli go) c) (go xs)
{-# INLINE hoistZombie #-}
