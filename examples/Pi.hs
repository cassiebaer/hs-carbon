module Main where

import Control.Monad (liftM2)
import Control.Monad.MonteCarlo
import Data.Summary.Bool
import System.Random.TF

mcSquareD :: RandomGen g => MonteCarlo g (Double,Double)
mcSquareD = liftM2 (,) (randomR (-1,1)) (randomR (-1,1))

inUnitCircle :: RandomGen g => MonteCarlo g Bool
inUnitCircle = do
    (x,y) <- mcSquareD
    return $ x*x + y*y <= 1

noRuns :: Int
noRuns = 1000000

main :: IO ()
main = do
    g <- newTFGen
    let s = experimentP inUnitCircle noRuns (noRuns `div` 200) g :: BoolSumm
    let (m,se) = (4*sampleMean s, 4*sampleSE s)
    putStrLn $ "Pi is probably between " ++ show (m-se,m+se)