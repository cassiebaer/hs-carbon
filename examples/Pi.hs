module Main where

import Control.Monad (liftM2)
import Control.Monad.MonteCarlo
import Data.Summary.Bool
import System.Environment
import System.Random
import System.Random.TF

mcSquareD :: RandomGen g => MonteCarlo g (Double,Double)
mcSquareD = liftM2 (,) (mcUniformR (-1,1)) (mcUniformR (-1,1))

inUnitCircle :: RandomGen g => MonteCarlo g Bool
inUnitCircle = do
    (x,y) <- mcSquareD
    return $ x*x + y*y <= 1

noRuns :: Int
noRuns = 10000000

main :: IO ()
main = do
    args <- getArgs
    g <- newTFGen
    let ex = case args of
                 ["-seq"] -> experimentS inUnitCircle noRuns g
                 ["-par",chSize] -> experimentP inUnitCircle noRuns (read chSize) g
                 _ -> error "Use -seq or -par chunksize"
    let (Summary s t) = ex
    print $ (4 * (fromIntegral s / fromIntegral t) :: Double)
    return ()