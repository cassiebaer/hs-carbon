module Main where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.MonteCarlo
import System.Random.TF

import Graphics.Gloss hiding (Point)

----------------------------------------------------------------
-- Data
----------------

rho :: Float
rho = 1 -- g / cm^3

massCoef :: Energy -> Float
massCoef x = 3.031E-02 / (1-(x/5001)) -- water @ 5MeV

absCoef :: Energy -> Float
absCoef x = 1.915E-02 / (1-(x/5001))-- water @ 5MeV

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

type Simulation = WriterT [(Point,Energy)] (StateT ParticleState (MonteCarlo TFGen))

eta :: Simulation Float
eta = lift (lift mcUniform)

etaR :: (Float,Float) -> Simulation Float
etaR bounds = lift (lift (mcUniformR bounds))

fly :: Simulation ()
fly = do
    eta' <- eta
    (PS i en (x,y) (ux,uy)) <- get
    let s = -(log eta' / mu_t en)
    put (PS i en (x+ux*s,y+uy*s) (ux,uy))

loop :: Simulation ()
loop = do
    fly
    scatter
    cond <- terminated
    if cond then return () else loop

exit :: Simulation Point
exit = gets curPos >>= return

scatter :: Simulation ()
scatter = do
    (PS i en (x,y) (ux,uy)) <- get
    let deltaW = mu_en en / mu_t en * en
    (ux',uy') <- if en <= 1000
                 then liftM2 (,) (etaR (-1,1)) (etaR (-1,1))
                 else do
                         dux <- etaR (-0.5,0.5)
                         duy <- etaR (-0.5,0.5)
                         return (ux+dux,uy+duy)
    tell [((x,y),deltaW)]
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
    --let bs = runMC (replicateM 10 (evalStateT (runWriterT loop) psInit)) g
    let bs = experimentP (evalStateT (execWriterT loop) psInit) 100000 1000 g :: [[(Point,Energy)]]
    displayResults bs

displayResults res = display (InWindow "Simulation Results" (800,800) (200,200))
                         blue results
  where color = makeColor8 255 0 0 10
        results = mconcat $ map (\path -> Color color $ Line (map fst path)) res