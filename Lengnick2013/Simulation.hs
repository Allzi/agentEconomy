{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Simulation where
import Control.Lens
import Control.Monad.State.Strict
import Data.Random
import System.Random

import SimUtils
import AgentTypes
import Debug.Trace



-- * Parameters
householdN, firmN, seed, duration :: Int
-- |The number of households, default is 1000.
householdN = 1000
-- |The number of firms, default is 100.
firmN      = 100
-- |The random seed of the simulation.
seed       = 1
-- |The length of the simulation.
duration   = 1000

-- * State

-- |Shorter name for our state.
-- Under the state monad is RVar from random-fu as a handy source of randomness.
type Simulation = StateT SimState RVar

-- |The state of the simulation.
-- Holds the agents, households and firms, with their ids.
data SimState = SimState {
    _households     :: !HMap,
    _householdIds   :: [Hid],
    _firms          :: !FMap,
    _firmIds        :: [Fid],
    _timer          :: !Int
    }

makeLenses ''SimState

-- |'startSim' is the starting state of simulation.
startSim :: SimState
startSim = SimState {
    _households     = makeMapWith hids makeHousehold,
    _householdIds   = hids,
    _firms          = makeMapWith fids makeFirm,
    _firmIds        = fids,
    _timer          = 0
    } 
  where
    hids = [0..(householdN-1)]
    fids = [0..(firmN-1)] 


-- * Utility functions 

mapMh :: (Household -> Simulation Household) -> Simulation ()
mapMh sim = do
    hs <- use households
    hs' <- mapMOf traverse sim hs
    households .= hs'

mapMf :: (Firm -> Simulation Firm) -> Simulation ()
mapMf sim = do
    fs <- use firms
    fs' <- mapMOf traverse sim fs
    firms .= fs'

