module Main where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.MonteCarlo
import Control.Monad.RWS
import Control.DeepSeq
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

type Simulation = RWST (Float -> (Float,Float)) [(Point,Energy)] ParticleState (MonteCarlo TFGen)
--type Simulation = WriterT [(Point,Energy)] (StateT ParticleState (MonteCarlo TFGen))

getMu :: Simulation (Float,Float)
getMu = do
    en <- gets remEnergy
    (t,a) <- asks (\f -> f en)
    return (rho*t,rho*a)

eta :: Simulation Float
eta = lift mcUniform

etaR :: (Float,Float) -> Simulation Float
etaR bounds = lift (mcUniformR bounds)

fly :: Simulation ()
fly = do
    (PS i en (x,y) (ux,uy)) <- get
    (mu_t,_) <- getMu
    eta' <- eta
    let s = -(log eta' / mu_t)
    if isNaN x || isNaN ux then fail (show (PS i en (x,y) (ux,uy))) else return ()
    put (PS i en (x+ux*s,y+uy*s) (ux,uy))

loop :: Simulation ()
loop = do
    fly
    scatter
    en <- gets remEnergy
    if en < 10 then return () else loop

scatter :: Simulation ()
scatter = do
    (PS i en (x,y) (ux,uy)) <- get
    eta' <- eta
    (mu_t,mu_en) <- getMu
    let deltaW = mu_en * en / mu_t-- * eta'
    eta'' <- eta
    let angle = diffAngle en (en-deltaW) * if eta'' >= 0.5 then 1 else (-1)
    if isNaN angle then fail (show (diffAngle en (en-deltaW), deltaW,en)) else return ()
    let (ux',uy') = rotate (ux,uy) angle
    tell [((x,y),deltaW)]
    put (PS (i+1) (en-deltaW) (x,y) (ux',uy'))

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
noRuns = 100000

main :: IO ()
main = do
    g <- newTFGen
    fnist <- loadData "water.dat"
    --let bs = experimentP (evalRRWST loop fnist psInit)
    --                     noRuns (noRuns `div` 50) g :: [[(Point,Energy)]]
    let bs = experimentS (evalRRWST loop fnist psInit) noRuns g :: [[(Point,Energy)]]
    --displayResults bs
    print bs
    return ()

displayResults :: [[(Point, Energy)]] -> IO ()
displayResults res = display (InWindow "Sim." (800,800) (200,200))
                         white (results `mappend` Color white (Line [(0,-100),(0,0)]))
  where color' = makeColor8 0 0 0 10
        results = mconcat $ map (\path -> Color color' $ Line (map fst path)) res

evalRRWST :: (Monad m) => RWST r w s m a -> r -> s -> m w
evalRRWST m r s = do
    ~(_,_,w) <- runRWST m r s
    return w