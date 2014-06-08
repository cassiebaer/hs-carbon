{-# LANGUAGE TypeFamilies, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.MonteCarlo
-- Copyright   :  (c) Casper M. H. Holmgreen
-- License     :  MIT
-- 
-- Maintainer  :  cholmgreen@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- The goal of Carbon is to capture idiomatic usage patterns for Monte
-- Carlo simulations in an easy to use interface. Currently, only one
-- pattern is implemented but more are planned.
--
-- There are two main parts to the library:
--
-- * The 'MonteCarlo' monad for building simulations
--
-- * The 'Data.Result' type family for describing how to aggregate results
--
-- Running a simulation will require a basic understanding of how to use both.
--
-----------------------------------------------------------------------------

module Control.Monad.MonteCarlo
  (
    -- * Running Simulations
    -- $runningsims
    MonteCarlo
  , experimentS
  , experimentP
    -- * Building Simulations
    -- $buildingsims
  , runMC
  , mcUniform
  , mcUniformR
  , RandomGen -- export RandomGen for convenience
  )
where

import Control.Monad.State
import Control.Parallel
import Data.List (foldl')
import Data.Result
import System.Random

{- $runningsims #runningsims#
Running simulations should be done using the high level functions provided here.
Monte Carlo simulations are specified by two parts: a 'MonteCarlo' action and a type annotation describing how to aggregate the results.
See 'Data.Result' for more information.
-}

{- $buildingsims #buildingsims#
Simulations are built by structuring 'MonteCarlo' actions over top of eachother, in a manner similar to parser combinators.
This compositional approach makes it very easy to express complex simulations without sacrificing readability.
Additionally, more complex simulations can layer monad transformers overtop of 'MonteCarlo' to add support for State, RO-environments, etc.
-}

-- | The 'MonteCarlo' monad is just a 'Control.Monad.State' monad in disguise.
-- This allows us to thread the internal PRNG state through the simulation as we sample new numbers.
--
type MonteCarlo g = State g

-- | This is a high level function for running a full Monte Carlo simulation.
-- It takes a 'MonteCarlo' action, the number of observations to aggregate, and an instance of 'System.Random.RandomGen'.
-- The return value is dictated by the type family 'Data.Result'; a type annotation is required to specify how observations should be aggregated.
--
-- For example, given a 'MonteCarlo' action, mySim, with type:
--
-- > mySim :: RandomGen g => MonteCarlo g Bool
--
-- We can get radically different results with just a type annotation:
--
-- > experimentS mySimulation 100 g :: [Bool]
-- > experimentS mySimulation 100 g :: BoolSumm
--
experimentS :: (RandomGen g, Result s)
            => MonteCarlo g (Obs s) -> Int -> g -> s
experimentS m n g = let xs = runMC (replicateM n m) g
                     in foldl' addObs rzero xs

-- | This is a high level function for running a full Monte Carlo simulation in parallel.
-- It is identical to 'experimentS' except that it takes an additional Int argument representing the chunk size.
-- Determining a good chunk size is an art, but a good starting point may be to divide the number of runs by 200.
--
-- Note: you must compile an executable with the -threaded flag in order for sparks to run in parallel.
--
experimentP :: (RandomGen g, Result s)
            => MonteCarlo g (Obs s) -> Int -> Int -> g -> s
experimentP m n c g
    | c <= 0    = error "Chunk size must be positive"
    | n <= c    = experimentS m n g
    | otherwise = s `par` (ss `pseq` (s `rjoin` ss))
  where
    s  = experimentS m c g1
    ss = experimentP m (n-c) c g2
    !(!g1,!g2) = split g

runMC :: RandomGen g => MonteCarlo g a -> g -> a
runMC = evalState

mcNext :: RandomGen g => (g -> (a,g)) -> MonteCarlo g a
mcNext f = do
    !g <- get
    let !(!x,!g') = f g
    put g'
    return x

mcUniform :: (RandomGen g, Random a) => MonteCarlo g a
mcUniform = do
    !g <- get
    let !(!x,!g') = random g
    put g'
    return x

mcUniformR :: (RandomGen g, Random a) => (a,a) -> MonteCarlo g a
mcUniformR !bounds = mcNext (randomR bounds)
