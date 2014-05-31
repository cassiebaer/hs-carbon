{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Summary where

import Control.DeepSeq

--class Monoid s => Result s a where
--    addObs :: s -> a -> s -- Add an observation to the result

--instance NFData a => Result [a] a where
--    addObs xs x = x `deepseq` (x : xs)

class Result r where
    type Obs r :: *
    addObs :: r -> Obs r -> r
    rjoin  :: r -> r -> r
    rzero  :: r

instance NFData a => Result [a] where
    type Obs [a]    = a
    addObs r o  = o `deepseq` (o : r)
    rjoin              = (++)
    rzero              = []