{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
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
import Data.List (foldl')
import Data.Monoid
import Data.Summary
import System.Random

-- | XX: space leak? requires increased stack storage on Transport2.hs
-- | Skeleton for common usage
experimentS :: (RandomGen g, Result s a)
            => MonteCarlo g a -> Int -> g -> s
experimentS m n g = let xs = runMC (replicateM n m) g
                     in foldl' addObs mempty xs

-- | Parallel
experimentP :: (RandomGen g, Result s a)
            => MonteCarlo g a -> Int -> Int -> g -> s
experimentP m n c g
    | n <= c    = experimentS m n g
    | otherwise = s `par` (ss `pseq` mappend s ss)
  where
    s  = experimentS m c g1
    ss = experimentP m (n-c) c g2
    !(g1,g2) = split g

-- | Monad representing a MonteCarlo simulation using
--    RandomGen instance g and returning a value of type a
type MonteCarlo g = State g

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
mcUniformR !bounds = mcNext (randomR bounds)
