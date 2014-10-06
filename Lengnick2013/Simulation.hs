{-# LANGUAGE TemplateHaskell, FlexibleInstances, RankNTypes #-}
module Simulation where
import Control.Lens
import Control.Monad.State.Strict
import Data.Random

import AgentTypes



-- * Parameters
rateDropWait, householdN, firmN, seed, duration, burnIn, unempVisits, 
    daysInMonth, shopN :: Int
-- | The number of households, default is 1000.
householdN      = 1000
-- | The number of firms, default is 100.
firmN           = 100
-- | The random seed of the simulation.
seed            = 1
-- | The length of the simulation.
duration        = 500
-- | Burn-in duration, of which data is not collected.
burnIn          = 450
-- | Months of full staff before the firm starts to lower its wage. 
-- Default is 24.
rateDropWait    = 24
-- | The number of firms that an unemployed visits when searching for a job.
-- Default is 5.
unempVisits     = 5
-- | How many (working) days are in a month. Default is 21.
daysInMonth     = 7
-- | How many local shops (type A connections) a household has. Default is 7.
shopN           = 7

-- | MAGIC
wageAdj, priceAdj, priceAdjProb, resWageDrop, qSearchProb, pSearchProb, 
    diffToReplace,iLowerBound, iUpperBound, pLowerBound, pUpperBound, 
    productivity, unsatProb, satProb, consAlpha, endShopTresh,
    mBufferMult  :: Double
-- | Maximum relative size of a wage adjustment. Default is 0.019.
wageAdj         = 0.019
-- | Maximum relative size of a price adjustment. Default is 0.02.
priceAdj        = 0.02
-- | Probability to adjust price at all. Default is 0.75.
priceAdjProb    = 0.75
-- | How much reservation wage drops after a month of unemployment.
-- Default is 0.1.
resWageDrop     = 0.1
-- | Probability for households to replace a shop wich could not provide
-- enough goods last month. Default is 0.25.
qSearchProb     = 0.25
-- | Probability for households to search for cheaper shops.
-- Default is 0.25.
pSearchProb     = 0.25
-- | Difference in price needed for replacement. Default is 0.01.
diffToReplace   = 0.01
-- | Limit of size of inventory relative to demand, below which a new worker
-- is hired. Default is 0.25.
iLowerBound     = 0.25
-- | Upper limit, above which a worker is fired. Default is 1.
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
-- | A parameter to determine households monthly demand as a function of its
-- monthly demand. Default is 0.9.
consAlpha       = 0.9
-- | Amount of satisfied demand needed before household ends shopping.
-- Default is 0.95.
-- Shopping can also end because there is no longer shops left.
endShopTresh    = 0.95
mBufferMult     = 0.1

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
    _sDividends     :: !Money,
    _sHousWealth    :: !Money,
    _timer          :: !(Int, Int, Int)
    }

makeLenses ''SimState

-- |'startSim' is the starting state of simulation.
startSim :: SimState
startSim = SimState {
    _households     = makeMapWith hids makeHousehold,
    _householdIds   = hids,
    _firms          = makeMapWith fids makeFirm,
    _firmIds        = fids,
    _sDividends     = 0,
    _sHousWealth    = 0,
    _timer          = (0,0,0)
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


