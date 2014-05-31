{-# LANGUAGE TypeFamilies #-}

module Data.Summary where

import Data.Result (Result(..))

class Result s => Summary s where
    sampleMean :: s -> Double
    sampleSE   :: s -> Double
    sampleSize :: s -> Int