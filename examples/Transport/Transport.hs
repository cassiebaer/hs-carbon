{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.MonteCarlo
import Control.Monad.Loops
import Control.DeepSeq
import Control.Exception
import System.Random.TF
import Transport.NISTData
import Data.List (foldl')

import Graphics.Gloss hiding (Point, rotate)

----------------------------------------------------------------
-- Datatypes
----------------

data ParticleState = PS
                     {
                       noColls   :: !Int
                     , remEnergy :: !Energy
                     , curPos    :: !Point
                     , curDir    :: !Angle
                     , path      :: [(Point,Energy)]
                     } deriving (Show)
psInit :: ParticleState
psInit = PS 0 5000 (0,0) (0,1) []

type Energy = Float
type Point = (Float,Float)
type Angle = (Float,Float)

----------------------------------------------------------------
-- MonteCarlo
----------------

type Simulation = ReaderT (Float -> (Float,Float)) (StateT ParticleState (MonteCarlo TFGen))

-- Helper function for getting the cross-section data for the current energy
getMu :: Simulation (Float,Float)
getMu = do
    en <- gets remEnergy
    (t,a) <- asks (\f -> f en)
    return (rho*t,rho*a)
  where rho = 1 -- g/cm^3 (water)

-- Helper functions for sampling random numbers
uniform :: Simulation Float
uniform = lift (lift random)
uniformR :: (Float,Float) -> Simulation Float
uniformR bounds = lift (lift (randomR bounds))

-- Flies the particle some random distance with prob. according to
--  cross-section data
fly :: Simulation ()
fly = do
    (PS i en (x,y) (ux,uy) ps) <- get
    (mu_t,_) <- getMu
    !eta <- uniform
    let s = -(log eta / mu_t)
    put (PS i en (x+ux*s,y+uy*s) (ux,uy) ps)

-- The main loop responsible for a single photon's lifetime
loop :: Simulation [(Point,Energy)]
loop = do
    untilM_ (fly >> scatter) isBelowCutoff
    exit

-- Terminates a particle if its energy is below the cutoff
isBelowCutoff :: Simulation Bool
isBelowCutoff = do
    en <- gets remEnergy
    return $ en < 10

-- Returns the path stored in the ParticleState
exit :: Simulation [(Point,Energy)]
exit = do
    ps <- gets path
    return $ ps

-- Randomly determines whether the scattering event scatters left or right
_scatterDir :: Simulation Float
_scatterDir = do
    eta <- uniform
    return $ if eta >= 0.5 then 1 else (-1)

-- Scattering event; compute scattering angle, record collision site
scatter :: Simulation ()
scatter = do
    (PS i en (x,y) (ux,uy) ps) <- get
    (mu_t,mu_en) <- getMu
    let deltaW = mu_en * en / mu_t
    dir <- _scatterDir
    let angle = diffAngle en (en-deltaW) * dir
    let (ux',uy') = rotate (ux,uy) angle
    put (PS (i+1) (en-deltaW) (x,y) (ux',uy') (ps++[((x,y),deltaW)]))

-- Computes the angle to rotate based on energy exchanged in coll.
diffAngle :: Energy -> Energy -> Float
diffAngle en en' = acos $ 1 - 0.511 * (1/en' - 1/en) -- Knuth

-- Rotates a vector
rotate :: Point -> Float -> Point
rotate (x,y) th = (x*cos th - y*sin th, x*sin th + y*cos th)

----------------------------------------------------------------
-- Main
----------------

noRuns :: Int
noRuns = 10000

main :: IO ()
main = do
    g <- newTFGen
    fnist <- loadData "water.dat"
    let unrolled = evalStateT (runReaderT loop fnist) psInit
    let bs = experimentP (unrolled)
                         noRuns (noRuns `div` 200) g :: [[(Point,Energy)]]
    evaluate (rnf bs)
    let lengthF = fromIntegral . length
    let avgCol = (foldl' (+) 0 (map lengthF bs)) / lengthF bs :: Double
    putStrLn $ "Average number of collisions: " ++ show avgCol
    displayResults bs

displayResults :: [[(Point, Energy)]] -> IO ()
displayResults res = display (InWindow "Sim." (800,800) (200,200))
                         white (results `mappend` Color white (Line [(0,-100),(0,0)]))
  where color' = makeColor8 0 0 0 100
        results = mconcat $ map (\p -> Color color' $ Line (map fst p)) res
