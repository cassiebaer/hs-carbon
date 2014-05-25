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

instance Result Bool Summary where
    addObs (Summary s t) True = Summary (s+1) (t+1)
    addObs (Summary s t) False = Summary s (t+1)