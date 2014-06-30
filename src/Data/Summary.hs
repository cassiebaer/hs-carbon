{-# LANGUAGE TypeFamilies #-}

module Data.Summary where

-- | Many Monte Carlo simulations require statistical analysis of the results.
-- Any 'Data.Result' instances which can be described statistically should be made instances of 'Summary'.
class Summary s where
    -- | Compute the mean of the aggregated observations
    sampleMean :: s -> Double
    -- | Compute the std. error of the aggregated observations
    sampleSE   :: s -> Double
    -- | Compute the variance of the aggregated observations
    sampleVar  :: s -> Double
    -- | Compute the standard deviation of the aggregated observations
    sampleSD   :: s -> Double
    -- | Return the number of observations aggregated
    sampleSize :: s -> Int
