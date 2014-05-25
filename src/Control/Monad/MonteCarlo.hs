module Control.Monad.MonteCarlo
  (
    MonteCarlo
  , experimentS
  , experimentM
  , runMC
  , mcUniform
  , mcUniformR
  )
where

import Control.Monad.State
import Data.Monoid
import System.Random

-- | Skeleton for common usage
experimentS :: RandomGen g => MonteCarlo g a -> Int -> g -> [a]
experimentS m n g = runMC (replicateM n m) g

-- | Skeleton for common usage
experimentM :: (RandomGen g, Monoid s)
            => MonteCarlo g a -> Int -> g -> (a -> s) -> s
experimentM m n g f = let xs = runMC (replicateM n m) g
                       in mconcat (map f xs)

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
