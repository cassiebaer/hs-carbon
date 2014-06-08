module Main where

import Control.Monad.MonteCarlo
import Data.Summary.Bool
import System.Random.TF

----------------------------------------------------------------
-- Example: Integrate sin(x) from 0 to pi
----------------

bounds :: ((Double,Double),(Double,Double))
bounds = ((0,pi),(0,1))

isUnderCurve :: RandomGen g => (Double -> Double) -> MonteCarlo g Bool
isUnderCurve f = do
    x <- randomR (fst bounds)
    y <- randomR (snd bounds)
    return $ y <= f x

noRuns :: Int
noRuns = 1000000

main :: IO ()
main = do
    g <- newTFGen
    let s = experimentP (isUnderCurve sin) noRuns 200000 g :: BoolSumm
    let ((_,r),(_,u)) = bounds
    print $ sampleMean s * r * u