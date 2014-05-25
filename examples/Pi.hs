module Main where

import Control.Monad (liftM2)
import Control.Monad.MonteCarlo
import Data.List (foldl')
import Data.Monoid (Monoid(..))
import System.Environment
import System.Random
import System.Random.TF

mcSquareD :: RandomGen g => MonteCarlo g (Double,Double)
mcSquareD = liftM2 (,) (mcUniformR (-1,1)) (mcUniformR (-1,1))

inUnitCircle :: RandomGen g => MonteCarlo g Bool
inUnitCircle = do
    (x,y) <- mcSquareD
    return $ x*x + y*y <= 1

----------------------------------------------------------------
-- Summary Construct
----------------

data TrueCount = TC !Int !Int -- Succ / Total

instance Monoid TrueCount where
    mempty = TC 0 0
    mappend (TC s t) (TC s' t') = TC (s+s') (t+t')
    mconcat = foldl' mappend mempty

----------------------------------------------------------------
-- Main
----------------

noRuns :: Int
noRuns = 10000000

toTC :: Bool -> TrueCount
toTC True = TC 1 1
toTC False = TC 0 1

main :: IO ()
main = do
    args <- getArgs
    g <- newTFGen
    let ex = case args of
                 ["-seq"] -> experimentS inUnitCircle noRuns g toTC
                 ["-par",chSize] -> experimentP inUnitCircle noRuns (read chSize) g toTC
                 _ -> error "Use -seq or -par chunksize"
    let (TC s t) = ex
    print $ (4 * (fromIntegral s / fromIntegral t) :: Double)
    return ()