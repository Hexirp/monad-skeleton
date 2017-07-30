{-# LANGUAGE GADTs #-}

import Prelude
import qualified Criterion.Main as Cri
import Control.Applicative
import Control.Monad.Skeleton
import Control.Monad.Zombie

import qualified Old as Old

data PF a where
 PF :: PF ()

type P a = Zombie PF a

zero_and_one :: P ()
zero_and_one = return () <|> liftZ PF

suite :: Int -> P ()
suite n = case compare n 0 of
 LT -> error "negative!"
 EQ -> return ()
 GT -> suite (n - 1) >> zero_and_one

run :: P () -> [Int]
run x = foldr r [] (disembalm x) where
 r :: MonadView PF (Zombie PF) () -> [Int] -> [Int]
 r (Return ()) xs = 0 : xs
 r (PF :>>= f) xs = (map (+ 1) $ run $ f ()) ++ xs

bench :: Int -> [Int]
bench = run . suite

data OF a where
 OF :: OF ()

type O a = Old.Zombie OF a

zero_and_one_o :: O ()
zero_and_one_o = return () <|> Old.liftZ OF

suite_o :: Int -> O ()
suite_o n = case compare n 0 of
 LT -> error "negative!"
 EQ -> return ()
 GT -> suite_o (n - 1) >> zero_and_one_o

run_o :: O () -> [Int]
run_o x = foldr r [] (Old.disembalm x) where
 r :: MonadView OF (Old.Zombie OF) () -> [Int] -> [Int]
 r (Return ()) xs = 0 : xs
 r (OF :>>= f) xs = (map (+ 1) $ run_o $ f ()) ++ xs

bench_o :: Int -> [Int]
bench_o = run_o . suite_o

main :: IO ()
main = Cri.defaultMain [
 Cri.bgroup "bench" $ [ Cri.bench "new_12" $ Cri.nf bench 12
            , Cri.bench "old_12" $ Cri.nf bench_o 12
            , Cri.bench "new_14" $ Cri.nf bench 14
            , Cri.bench "old_14" $ Cri.nf bench_o 14
            ]
 ]
