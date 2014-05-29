{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Summary where

import Data.Monoid
import Control.DeepSeq

class Monoid s => Result s a where
    addObs :: s -> a -> s -- Add an observation to the result

instance NFData a => Result [a] a where
    addObs xs x = x `deepseq` (x : xs)