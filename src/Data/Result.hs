{-# LANGUAGE TypeFamilies #-}

module Data.Result
  (
    -- * Describing Simulation Results with Type Annotations
    -- $description
    Result(..)
  )
where

import Control.DeepSeq

{- $description #description#
Carbon simulations are built up from 'Control.Monad.MonteCarlo' actions.
A 'MonteCarlo' action describes how to arrive at an observation, but not how to aggregate observations.
This functionality is specified with a type annotation telling Haskell which instance of the type family 'Result' should be used.

For example, given a 'MonteCarlo' action, mySim, with type:

> mySim :: RandomGen g => MonteCarlo g Bool

We get different results based on the instance of 'Result' chosen:

> experimentS mySimulation 100 g :: [Bool]
> experimentS mySimulation 100 g :: BoolSumm
-}

-- | Result is the type family used to describe the aggregation techniques to be used in a Monte Carlo simulation.
-- Instances of Result should specify the type of a single observation and how to include one.
-- The value of a 'Result' without any observations should be specified.
-- Additionally, 'Result's should be joinable.
--
-- Note that almost all instances of 'Result' will be monoidal.
class Result r where
    -- | The type of a single observation
    type Obs r :: *
    -- | How to add a single observation to the result
    addObs :: r -> Obs r -> r
    -- | How to join two results together
    rjoin  :: r -> r -> r
    -- | The value of a result without any observations
    rzero  :: r

-- | One common aggregation technique is appending observations to a list.
-- In the interest of preventing thunks from building up, we force deep evaluation of observations.
instance NFData a => Result [a] where
    type Obs [a] = a
    addObs r o   = o `deepseq` (o : r)
    rjoin        = mappend
    rzero        = mempty
