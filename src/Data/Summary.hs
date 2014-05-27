{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Summary where

import Data.Monoid

class Monoid s => Result a s where
    addObs :: s -> a -> s -- Add an observation to the result

instance Result a [a] where
    addObs xs x = xs ++ [x]