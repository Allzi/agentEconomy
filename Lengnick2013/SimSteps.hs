{-# LANGUAGE TemplateHaskell #-}
module SimSteps where
import Prelude hiding (foldl, maximum, minimum)
import Control.Lens
import Control.Monad.State.Strict
import Data.RVar
import qualified Data.Vector.Unboxed as V

import AgentTypes
import Simulation
import SimUtils
import Month

-- | Runs the simulation with a seed.
simulateSeed :: Int -> SimData
simulateSeed seed = rev
  where
    simData = evalState runSimulation (startSim seed)
    rev = fmap reverse simData

-- | Loops simulation, returning the data.
-- Used in simulateSeed.
runSimulation :: Simulation SimData
runSimulation  = do
    runYears
    d <- use sData
    return d

-- | Runs one round of simulation step by step. 
runYears :: Simulation ()
runYears = do
    timer._1 += 1
    t <- use $ timer._1
    when (t == 1) initSimulation
    runMonths
    when (t < duration) runYears

-- | Called before first year, sets obligatory random links betwen agents.
initSimulation :: Simulation ()
initSimulation = do
    fids <- use $ firmIds
    households <%=> getShops fids
    households <%=> initJob fids
  where
    getShops fids h = do
        rfids <- sampleRVar $ shuffleV fids
        return $ h&hShops .~ take shopN (V.toList rfids)
    initJob fids h = do
        rfid <- sampleRVar $ randomElementV fids
        Just f <- use $ firms.at rfid
        let f' = f&fWorkers     %~ ((h^.hID):)
                  &fSize        +~ 1
                  &fSizeTarget  +~ 1
        firms.ix rfid .= f'
        return $ h&hEmployer .~ Just rfid
                  &hResWage  .~ (f^.fWageRate)

