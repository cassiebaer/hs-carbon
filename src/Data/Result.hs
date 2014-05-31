{-# LANGUAGE TypeFamilies #-}

module Data.Result where

import Control.DeepSeq

class Result r where
    type Obs r :: *
    addObs :: r -> Obs r -> r
    rjoin  :: r -> r -> r
    rzero  :: r

instance NFData a => Result [a] where
    type Obs [a] = a
    addObs r o   = o `deepseq` (o : r)
    rjoin        = (++)
    rzero        = []