{-# LANGUAGE TemplateHaskell #-}
module SimSteps where
import Prelude hiding (foldl)
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.IntMap as Map
import Data.Foldable
import Data.List hiding (foldl)
import Debug.Trace
import System.Random
import Data.Random

import AgentTypes
import Simulation
import SimUtils
import Month

-- | Purely runs the simulation with a seed.
simulateSeed :: Int -> [SimData]
simulateSeed seed = rev
  where
    (dataList, _) = sampleState (runSimulation [] startSim) (mkStdGen seed)
    rev = reverse dataList


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
    t <- use $ timer._1
    when (t == 0) initSimulation
    timer._1 += 1
    runMonths 
    collectData

-- | Called before first year, sets obligatory random links betwen agents.
initSimulation :: Simulation ()
initSimulation = do
    fids <- use $ firmIds
    households <$=> getShops fids
  where
    getShops fids h = do
        rfids <- lift $ shuffleNofM shopN firmN fids
        return $ h&hShops .~ (zip rfids (repeat 0))

-- | All the data is collected here.
collectData :: Simulation SimData
collectData =  do
    t <- use $ timer._1
    hw <- use sHousWealth
    divid <- use sDividends
    hs <- use households
    fs <- use firms
    let une = foldl calcUne 0 hs
        p = (foldl (\acc f -> acc + f^.fPrice) 0 fs) / fromIntegral firmN
        usdem = (foldl (\acc f -> acc + f^.fMDemand) 0 fs) / fromIntegral firmN
        op = foldl (\acc f -> acc + f^.fOpenPositions) 0 fs
        ow = (foldl (\acc f -> acc + f^.fWageRate) 0 fs) / fromIntegral firmN
        aw = (foldl (\acc h -> acc + h^.hResWage) 0 hs) / fromIntegral householdN
    return [("UnemployedN", une),
            ("Pricelevel", p),
            ("Open_Positions", fromIntegral op),
            ("Unsatisfied_Demand", usdem),
            ("Offered_Wage", ow),
            ("Accepted_Wage", aw),
            ("Household_Wealh", hw),
            ("Aggregate Dividends", divid),
            ("Time", fromIntegral t)]
  where
    calcUne acc h = if h^.hEmployer == Nothing
        then acc + 1
        else acc

