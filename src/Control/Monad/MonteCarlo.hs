module Control.Monad.MonteCarlo
  (
    MonteCarlo
  , experimentS
  , experimentP
  , runMC
  , mcUniform
  , mcUniformR
  )
where

import Control.Monad.State
import Control.Parallel
import Data.Monoid
import System.Random

-- | Skeleton for common usage
experimentS :: (RandomGen g, Monoid s)
            => MonteCarlo g a -> Int -> g -> (a -> s) -> s
experimentS m n g f = let xs = runMC (replicateM n m) g
                       in mconcat (map f xs)

-- | Parallel
experimentP :: (RandomGen g, Monoid s)
            => MonteCarlo g a -> Int -> Int -> g -> (a -> s) -> s
experimentP m n c g f
    | n <= c    = experimentS m n g f
    | otherwise = s `par` (ss `pseq` mappend s ss)
  where
    s  = experimentS m c g1 f
    ss = experimentP m (n-c) c g2 f
    (g1,g2) = split g

-- | Monad representing a MonteCarlo simulation using
--    RandomGen instance g and returning a value of type a
type MonteCarlo g a = State g a

runMC :: RandomGen g => MonteCarlo g a -> g -> a
runMC = evalState

mcNext :: RandomGen g => (g -> (a,g)) -> MonteCarlo g a
mcNext f = do
    g <- get
    let (x,g') = f g
    put g'
    return x

mcUniform :: (RandomGen g, Random a) => MonteCarlo g a
mcUniform = mcNext random

mcUniformR :: (RandomGen g, Random a) => (a,a) -> MonteCarlo g a
mcUniformR bounds = mcNext (randomR bounds)
