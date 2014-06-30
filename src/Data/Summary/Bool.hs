{-# LANGUAGE TypeFamilies #-}

module Data.Summary.Bool
  (BoolSumm, Summary(..), boolSumm)
  where

import Data.Result (Result(..))
import Data.Summary (Summary(..))
import Data.List (foldl')
import Control.DeepSeq (NFData(..))

-- | A 'BoolSumm' counts the number of True and all events observed.
data BoolSumm = BoolSumm {
                  _noSuccess :: !Int
                , _noTotal   :: !Int
                } deriving (Show)

instance NFData BoolSumm

boolSumm :: [Bool] -> BoolSumm
boolSumm = foldl' addObs rzero

instance Result BoolSumm where
    type Obs BoolSumm = Bool
    addObs (BoolSumm s t) True = BoolSumm (s+1) (t+1)
    addObs (BoolSumm s t) False = BoolSumm s (t+1)
    rjoin (BoolSumm s t) (BoolSumm s' t') = BoolSumm (s+s') (t+t')
    rzero = BoolSumm 0 0

instance Summary BoolSumm where
    sampleMean (BoolSumm s t) = fromIntegral s / fromIntegral t
    sampleSE s = sqrt (p * (1 - p) / n)
      where
        p = sampleMean s
        n = fromIntegral $ sampleSize s
    sampleSize (BoolSumm _ t) = t