{-# LANGUAGE TemplateHaskell, FlexibleInstances, RankNTypes #-}
module Simulation where
import Control.Lens
import Control.Monad.State.Strict
import Data.Random
import System.Random
import Control.Applicative
import qualified Data.Traversable as T

import SimUtils
import AgentTypes
import Debug.Trace



-- * Parameters
rateDropWait, householdN, firmN, seed, duration, unempVisits, 
    daysInMonth :: Int
-- | The number of households, default is 1000.
householdN      = 1000
-- | The number of firms, default is 100.
firmN           = 100
-- | The random seed of the simulation.
seed            = 1
-- | The length of the simulation.
duration        = 1000
-- | Months of full staff before the firm starts to lower its wage. 
-- Default is 24.
rateDropWait    = 24
-- | The number of firms that an unemployed visits when searching for a job.
-- Default is 5.
unempVisits     = 5
-- | How many (working) days are in a month. Default is 21.
daysInMonth     = 21

-- | MAGIC
wageAdj, priceAdj, iLowerBound, iUpperBound, pLowerBound, pUpperBound, 
    productivity, unsatProb, satProb  :: Double
wageAdj         = 0.019
priceAdj        = 0.02
iLowerBound     = 0.25
iUpperBound     = 1
-- | How much above marginal cost a price is lowered. Default is 1.15.
pUpperBound     = 1.15
-- | How much below marginal cost a price is raised. Default is 1.025.
pLowerBound     = 1.025
productivity    = 3
-- | Probability that unsatisfied worker searches for a better job. 
-- Default is 1.
unsatProb       = 1
-- | Probability that satisfied worker searces for a better job. 
-- Deafault is 0.1.
satProb         = 0.1

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

mapMSim, (<$=>) :: Traversable t => 
    (Lens' SimState (t a)) ->
    (a -> Simulation a) -> 
    Simulation ()
mapMSim l sim = (l .=) =<< (use l <&> traverse sim & join)

infixl 4 <$=> 
(<$=>) = mapMSim


