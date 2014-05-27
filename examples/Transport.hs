module Main where

import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.MonteCarlo
import System.Random.TF

----------------------------------------------------------------
-- Data
----------------

rho :: Float
rho = 1 -- g / cm^3

massCoef :: Energy -> Float
massCoef _ = 3.031E-02 -- water @ 5MeV

absCoef :: Energy -> Float
absCoef _ = 1.915E-02 -- water @ 5MeV

mu_t :: Energy -> Float
mu_t x = massCoef x * rho

mu_en :: Energy -> Float
mu_en x = absCoef x * rho

----------------------------------------------------------------
-- Datatypes
----------------

data ParticleState = PS
                     {
                       noColls   :: Int
                     , remEnergy :: Energy
                     , curPos    :: Point
                     , curDir    :: Angle
                     } deriving (Show)
psInit :: ParticleState
psInit = PS 0 5000 (0,0) (0,1)
type Energy = Float
type Point = (Float,Float)
type Angle = (Float,Float)
type History = [Point]

----------------------------------------------------------------
-- MonteCarlo
----------------

type Simulation = StateT ParticleState (MonteCarlo TFGen)

eta :: Simulation Float
eta = lift mcUniform

etaR :: (Float,Float) -> Simulation Float
etaR bounds = lift (mcUniformR bounds)

fly :: Simulation ()
fly = do
    eta' <- eta
    (PS i en (x,y) (ux,uy)) <- get
    let s = -(log eta' / mu_t en)
    put (PS i en (x+ux*s,y+uy*s) (ux,uy))

loop :: Simulation Point
loop = do
    fly
    scatter
    cond <- terminated
    if cond then exit else loop

exit :: Simulation Point
exit = gets curPos >>= return

scatter :: Simulation ()
scatter = do
    (PS i en (x,y) (ux,uy)) <- get
    let deltaW = mu_en en / mu_t en * en
    (ux',uy') <- if en <= 1000
                 then liftM2 (,) (etaR (-1,1)) (etaR (-1,1))
                 else do
                         dux <- etaR (-1,1)
                         duy <- etaR (-1,1)
                         return (ux+dux,uy+duy)
    put (PS (i+1) (en-deltaW) (x,y) (ux',uy'))

terminated :: Simulation Bool
terminated = do
    en <- gets remEnergy
    return $ en < 500

----------------------------------------------------------------
-- Main
----------------

main :: IO ()
main = do
    g <- newTFGen
    let bs = runMC (replicateM 10 (evalStateT loop psInit)) g
    print bs
    return ()