module Main where

import Control.Monad.MonteCarlo
import Data.Summary.Bool
import System.Random (RandomGen(..))
import System.Random.TF

----------------------------------------------------------------
-- Example: Integrate sin(x) from 0 to pi
----------------

rightBound :: Double
rightBound = pi

upperBound :: Double
upperBound = 1

isUnderCurve :: RandomGen g => (Double -> Double) -> MonteCarlo g Bool
isUnderCurve f = do
    x <- mcUniformR (0,rightBound)
    y <- mcUniformR (0,upperBound)
    return $ y <= f x

noRuns :: Int
noRuns = 10000000

main :: IO ()
main = do
    g <- newTFGen
    let (Summary s t) = experimentP (isUnderCurve sin) noRuns 200000 g
    print $ fromIntegral s / fromIntegral t * (upperBound * rightBound)
    return ()