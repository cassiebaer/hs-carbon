module Main where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.MonteCarlo
import System.Random.TF

import Graphics.Gloss hiding (Point, rotate)

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
    en <- gets remEnergy
    if en < 100 then return () else loop

scatter :: Simulation ()
scatter = do
    (PS i en (x,y) (ux,uy)) <- get
    eta' <- eta
    let deltaW = mu_en en / mu_t en * en * eta'
    eta'' <- eta
    let angle = diffAngle deltaW * if eta'' >= 0.5 then 1 else (-1)
    let (ux',uy') = rotate (ux,uy) angle
    tell [((x,y),deltaW)]
    put (PS (i+1) (en-deltaW) (x,y) (ux',uy'))

waveLength :: Energy -> Float
waveLength en = 1240 / (en * 1000) -- en given in keV

diffAngle :: Energy -> Float
diffAngle en = acos $ 1 - waveLength en / 0.00243 -- en given in keV

rotate :: Point -> Float -> Point
rotate (x,y) th = (x*cos th - y*sin th, x*sin th + y*cos th)

----------------------------------------------------------------
-- Main
----------------

main :: IO ()
main = do
    g <- newTFGen
    let bs = experimentP (evalStateT (execWriterT loop) psInit)
                         10 1000 g :: [[(Point,Energy)]]
    displayResults bs

displayResults :: [[(Point, Energy)]] -> IO ()
displayResults res = display (InWindow "Sim." (800,800) (200,200))
                         blue (results `mappend` Color white (Line [(0,-100),(0,0)]))
  where color' = makeColor8 255 0 0 200
        results = mconcat $ map (\path -> Color color' $ Line (map fst path)) res