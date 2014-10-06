{-# LANGUAGE TemplateHaskell #-}
module SimSteps where
import Prelude hiding (foldl, maximum, minimum)
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.IntMap as Map
import Data.Foldable
import Data.List hiding (foldl, maximum, minimum)
import Debug.Trace
import System.Random
import Data.Random

import AgentTypes
import Simulation
import SimUtils
import Month

-- | Purely runs the simulation with a seed.
simulateSeed :: Int -> [SimData]
simulateSeed seed = outputData
  where
    (dataList, _) = sampleState (runSimulation [] startSim) (mkStdGen seed)
    rev = reverse dataList
    outputData = drop burnIn rev


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
    households <$=> initJob fids
  where
    getShops fids h = do
        rfids <- lift $ shuffleNofM shopN firmN fids
        return $ h&hShops .~ (zip rfids (repeat 0))
    initJob fids h = do
        rfid <- lift $ randomElementN firmN fids
        Just f <- use $ firms.at rfid
        let f' = f&fWorkers %~ ((h^.hID):)
                  &fSize    +~ 1
        firms.ix rfid .= f'
        return $ h&hEmployer .~ Just rfid
                  &hWage     .~ Just (f^.fWageRate)

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
        invs = (foldl (\acc f -> acc + f^.fInventory) 0 fs) / fromIntegral firmN
        maxPrice = (maximum . fmap (\f -> f^.fPrice)) fs
        minPrice = (minimum . fmap (\f -> f^.fPrice)) fs
        maxSize = (maximum . fmap (\f -> fromIntegral (f^.fSize))) fs
        minSize = (minimum . fmap (\f -> fromIntegral (f^.fSize))) fs
    return [("UnemployedN", une),
            ("Pricelevel", p),
            ("Open_Positions", fromIntegral op),
            ("Unsatisfied_Demand", usdem),
            ("Offered_Wage", ow),
            ("Accepted_Wage", aw),
            ("Household_Wealh", hw),
            ("Aggregate Dividends", divid),
            ("Inventories", invs),
            ("maxPrice", maxPrice),
            ("maxSize", maxSize),
            ("minSize", minSize),
            ("minPrice", minPrice),
            ("Time", fromIntegral t)]
  where
    calcUne acc h = if h^.hEmployer == Nothing
        then acc + 1
        else acc

