{-# LANGUAGE TypeFamilies #-}

module Data.Summary.Double where

import Data.Result (Result(..))
import Data.Summary (Summary(..))
import Data.List (foldl')
import Control.DeepSeq (NFData(..))

-- | Computes running stats as demonstrated by
--    http://www.johndcook.com/skewness_kurtosis.html
data DoubleSumm = DoubleSumm {
                    _m1   :: !Double
                  , _m2   :: !Double
                  , _size :: !Int
} deriving (Show)

instance NFData DoubleSumm

doubleSumm :: [Double] -> DoubleSumm
doubleSumm = foldl' addObs rzero

instance Result DoubleSumm where
    type Obs DoubleSumm = Double
    addObs (DoubleSumm m v n) x = DoubleSumm m' v' n'
      where
        delta = x - m
        n' = n + 1
        m' = m + delta/fromIntegral n'
        v' = v + delta*(delta/fromIntegral n')*fromIntegral n
    rjoin (DoubleSumm m1 v1 n1) (DoubleSumm m2 v2 n2) = DoubleSumm m' v' n'
      where
        delta = m2 - m1
        n' = n1 + n2
        m' = (fromIntegral n1*m1 + fromIntegral n2*m2)/fromIntegral n'
        v' = v1+v2 + delta*delta*fromIntegral n1/fromIntegral n'*fromIntegral n2
    rzero = DoubleSumm 0 0 0

instance Summary DoubleSumm where
    sampleMean   = _m1
    sampleVar ds = _m2 ds / fromIntegral (_size ds - 1)
    sampleSD     = sqrt . sampleVar
    sampleSE ds  = sampleSD ds / sqrt (fromIntegral (_size ds))
    sampleSize   = _size
