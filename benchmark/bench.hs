{-# LANGUAGE GADTs #-}

import Prelude
import Control.Applicative
import Control.Monad.Skeleton
import Control.Monad.Zombie

data PF a where
 PF :: PF ()

type P a = Zombie PF a

zero_and_one :: P ()
zero_and_one = return () <|> liftZ PF

bench :: Int -> P ()
bench n = case compare n 0 of
 LT -> error "negative!"
 EQ -> return ()
 GT -> bench (n - 1) >> zero_and_one

run :: P () -> [Int]
run x = foldr r [] (disembalm x) where
 r :: MonadView PF (Zombie PF) () -> [Int] -> [Int]
 r (Return ()) xs = 0 : xs
 r (PF :>>= f) xs = (map (+ 1) $ run $ f ()) ++ xs
