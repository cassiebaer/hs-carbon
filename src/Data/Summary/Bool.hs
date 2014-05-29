{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Summary.Bool where

import Data.List (foldl')
import Data.Monoid
import Data.Summary

data Summary = Summary
                 {
                   noSuccess :: !Int
                 , noTotal   :: !Int
                 }

instance Monoid Summary where
    mempty = Summary 0 0
    mappend (Summary s t) (Summary s' t') = Summary (s+s') (t+t')
    mconcat = foldl' mappend mempty

instance Result Summary Bool where
    addObs (Summary s t) True = Summary (s+1) (t+1)
    addObs (Summary s t) False = Summary s (t+1)

sampleMean :: Summary -> Double
sampleMean (Summary s t) = fromIntegral s / fromIntegral t

sampleSize :: Summary -> Int
sampleSize (Summary _ t) = t

sampleSE :: Summary -> Double
sampleSE s = sqrt (p * (1 - p) / n)
  where
    p = sampleMean s
    n = fromIntegral $ sampleSize s