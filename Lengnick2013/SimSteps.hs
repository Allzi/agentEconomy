{-# LANGUAGE TemplateHaskell #-}
module SimSteps where
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.IntMap as Map
import Data.List
import Debug.Trace
import System.Random
import Data.Random

import AgentTypes
import Simulation
import SimUtils
import Month

-- | Purely runs the simulation with a seed.
simulateSeed :: Int -> [SimData]
simulateSeed seed = dataList
  where
    (dataList, _) = sampleState (runSimulation [] startSim) (mkStdGen seed)


-- | Loops simulation, returning the data.
-- Used in simulateSeed.
runSimulation :: [SimData] -> SimState -> RVar ([SimData])
runSimulation sds sim =
    if (sim^.timer._1 < duration)
        then do
            (sd, sim') <- runStateT simStep sim
            runSimulation (sd:sds) sim'
        else return sds

-- | Runs one round of simulation step by step. 
simStep :: Simulation SimData
simStep = do
    timer._1 += 1
    
    collectData

-- | All the data is collected here.
collectData :: Simulation SimData
collectData =  do
    a <- lift $ stdUniform
    return [("asd", a)]

