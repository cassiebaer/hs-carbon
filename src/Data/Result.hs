{-# LANGUAGE TypeFamilies #-}

module Data.Result where

import Control.DeepSeq
import Data.Monoid

class Result r where
    type Obs r :: *           -- type of obs.
    addObs :: r -> Obs r -> r -- add an observation
    rjoin  :: r -> r -> r     -- join two results
    rzero  :: r               -- result sans observations

instance NFData a => Result [a] where
    type Obs [a] = a
    addObs r o   = o `deepseq` (o : r)
    rjoin        = mappend
    rzero        = mempty