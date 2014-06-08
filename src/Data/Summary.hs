{-# LANGUAGE TypeFamilies #-}

module Data.Summary where

class Summary s where
    sampleMean :: s -> Double
    sampleSE   :: s -> Double
    sampleSize :: s -> Int