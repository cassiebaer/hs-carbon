{-# LANGUAGE TypeFamilies #-}

module Data.Summary.Bool where

import Data.List (foldl')
import Data.Summary (Result(..))

data BoolSumm = BoolSumm
                 {
                   noSuccess :: !Int
                 , noTotal   :: !Int
                 }

instance Result BoolSumm where
    type Obs BoolSumm = Bool
    addObs (BoolSumm s t) True = (BoolSumm (s+1) (t+1))
    addObs (BoolSumm s t) False = (BoolSumm s (t+1))
    rjoin (BoolSumm s t) (BoolSumm s' t') = BoolSumm (s+s') (t+t')
    rzero = BoolSumm 0 0

sampleMean :: BoolSumm -> Double
sampleMean (BoolSumm s t) = fromIntegral s / fromIntegral t

sampleSize :: BoolSumm -> Int
sampleSize (BoolSumm _ t) = t

sampleSE :: BoolSumm -> Double
sampleSE s = sqrt (p * (1 - p) / n)
  where
    p = sampleMean s
    n = fromIntegral $ sampleSize s