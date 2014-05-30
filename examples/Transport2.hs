{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.MonteCarlo
import Control.Monad.Loops
import Control.DeepSeq
import Control.Exception
import System.Random.TF
import NISTData

import Graphics.Gloss hiding (Point, rotate)

----------------------------------------------------------------
-- Data
----------------

rho :: Float
rho = 1

----------------------------------------------------------------
-- Datatypes
----------------

data ParticleState = PS
                     {
                       noColls   :: !Int
                     , remEnergy :: !Energy
                     , curPos    :: !Point
                     , curDir    :: !Angle
                     , path      :: ![(Point,Energy)]
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
--type Simulation = RWST (Float -> (Float,Float)) [(Point,Energy)] ParticleState (MonteCarlo TFGen)
--type Simulation = WriterT [(Point,Energy)] (StateT ParticleState (MonteCarlo TFGen))

getMu :: Simulation (Float,Float)
getMu = do
    en <- gets remEnergy
    (t,a) <- asks (\f -> f en)
    return (rho*t,rho*a)

eta :: Simulation Float
eta = lift (lift mcUniform)

etaR :: (Float,Float) -> Simulation Float
etaR bounds = lift (lift (mcUniformR bounds))

fly :: Simulation ()
fly = do
    (PS i en (x,y) (ux,uy) ps) <- get
    (mu_t,_) <- getMu
    eta' <- eta
    let s = -(log eta' / mu_t)
    put (PS i en (x+ux*s,y+uy*s) (ux,uy) ps)


loop :: Simulation [(Point,Energy)]
loop = do
    untilM_ (fly >> scatter) isBelowCutoff
    exit

isBelowCutoff :: Simulation Bool
isBelowCutoff = do
    en <- gets remEnergy
    return $ en < 10

exit :: Simulation [(Point,Energy)]
exit = do
    ps <- gets path
    return $ ps

scatter :: Simulation ()
scatter = do
    (PS i en (x,y) (ux,uy) ps) <- get
    (mu_t,mu_en) <- getMu
    eta' <- etaR (0,2)
    let deltaW = mu_en * en / mu_t * eta'
    eta'' <- eta
    let angle = diffAngle en (en-deltaW) * if eta'' >= 0.5 then 1 else (-1)
    let (ux',uy') = rotate (ux,uy) angle
    put (PS (i+1) (en-deltaW) (x,y) (ux',uy') (ps++[((x,y),deltaW)]))

waveLength :: Energy -> Float
waveLength en = 1240 / (en * 1000) -- en given in keV

diffAngle :: Energy -> Energy -> Float
--diffAngle en = acos $ 1 - waveLength en / 0.00243 -- en given in keV
diffAngle en en' = acos $ 1 - 0.511 * (1/en' - 1/en)

rotate :: Point -> Float -> Point
rotate (x,y) th = (x*cos th - y*sin th, x*sin th + y*cos th)

----------------------------------------------------------------
-- Main
----------------

noRuns :: Int
noRuns = 1000000

main :: IO ()
main = do
    g <- newTFGen
    fnist <- loadData "water.dat"
    --let fnist = \x -> (3.969E-02,2.281E-02)
    let unrolled = evalStateT (runReaderT loop fnist) psInit
    let bs = experimentP (unrolled)
                         noRuns (noRuns `div` 200) g :: [[(Point,Energy)]]
    --let bs = experimentS (unrolled) noRuns g :: [[(Point,Energy)]]
    evaluate (rnf bs)
    --displayResults bs
    return ()

displayResults :: [[(Point, Energy)]] -> IO ()
displayResults res = display (InWindow "Sim." (800,800) (200,200))
                         white (results `mappend` Color white (Line [(0,-100),(0,0)]))
  where color' = makeColor8 0 0 0 5
        results = mconcat $ map (\p -> Color color' $ Line (map fst p)) res
