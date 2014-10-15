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
import Control.DeepSeq

import AgentTypes
import Simulation
import SimUtils
import Month

-- | Purely runs the simulation with a seed.
simulateSeed :: Int -> [SimData]
simulateSeed seed = rev
  where
    (dataList, _) = sampleState (runSimulation startSim) (mkStdGen seed)
    rev = reverse dataList


-- | Loops simulation, returning the data.
-- Used in simulateSeed.
runSimulation :: SimState -> RVar [SimData]
runSimulation sim =
    if (sim^.timer._1 < duration) 
        then do
            (_, sim') <- runStateT simStep sim
            runSimulation sim'
        else return $ sim^.sData

-- | Runs one round of simulation step by step. 
simStep :: Simulation ()
simStep = do
    t <- use $ timer._1
    when (t == 0) initSimulation
    timer._1 += 1
    runMonths 

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
        let f' = f&fWorkers     %~ ((h^.hID):)
                  &fSize        +~ 1
                  &fSizeTarget  +~ 1
        firms.ix rfid .= f'
        return $ h&hEmployer .~ Just rfid
                  &hResWage  .~ (f^.fWageRate)

