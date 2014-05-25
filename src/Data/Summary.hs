{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Summary where

import Data.Monoid

class Monoid s => Result a s where
    addObs :: s -> a -> s -- Add an observation to the result