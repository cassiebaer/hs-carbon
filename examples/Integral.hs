module Main where

import Control.Monad.MonteCarlo
import Data.List
import Data.Monoid
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

data TrueCount = TC !Int !Int

instance Monoid TrueCount where
    mempty = TC 0 0
    mappend (TC a b) (TC c d) = TC (a+c) (b+d)
    mconcat = foldl' mappend mempty

toTC :: Bool -> TrueCount
toTC True = TC 1 1
toTC False = TC 0 1

noRuns :: Int
noRuns = 10000000

main :: IO ()
main = do
    g <- newTFGen
    let (TC s t) = experimentP (isUnderCurve sin) noRuns 200000 g toTC
    print $ fromIntegral s / fromIntegral t * (upperBound * rightBound)
    return ()