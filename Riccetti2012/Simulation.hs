{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Simulation where
import Control.Lens
import Control.Monad.State.Strict
import System.Random

import SimUtils
import AgentTypes
import Debug.Trace

-- Parameters:
seed, workerN, producerN, bankN, duration, producerTrials, workerTrials, 
    bankTrials, productivity :: Int
seed            = 1
workerN         = 500
producerN       = 80
bankN           = 10
producerTrials  = 16
workerTrials    = 100
bankTrials      = 2
duration        = 100
productivity    = 3

genericAdj, inventoryTreshold, riskPremium,
    reg1, reg2, reg3, incomeCons, wealthCons :: Double
genericAdj          = 0.05
inventoryTreshold   = 0.1
riskPremium         = 2
reg1                = 10
reg2                = 0.5
reg3                = 0.1
incomeCons          = 0.8
wealthCons          = 0.3

type Simulation = State SimState

data SimState = SimState {
    _workers        :: !WorkerMap,
    _workerIds      :: [Wid],
    _producers      :: !ProducerMap,
    _banks          :: !BankMap,
    _sRandoms       :: [Double],
    _timer          :: !Int,
    _cbInterest     :: !Double,
    _avgPrice       :: !Money,
    _avgInterest    :: !Money,
    _aggDividends   :: !Money,
    _cbMoney        :: !Money,
    _government     :: !Government
    }

makeLenses ''SimState

startSim :: [Double] -> SimState
startSim rands = SimState {
    _workers        = makeMapWith wids makeWorker,
    _workerIds      = wids,
    _producers      = makeMapWith pids makeProducer,
    _banks          = makeMapWith bids makeBank,
    _sRandoms       = rands,
    _timer          = 0,
    _cbInterest     = 0.01,
    _avgPrice       = 1,
    _avgInterest    = 0.01,
    _aggDividends   = 0,
    _cbMoney        = 10,
    _government     = makeGovernment
    } 
  where 
    wids = [0..(workerN-1)]
    pids = [0..(producerN-1)] 
    bids = [0..(bankN-1)]

instance RSim (State SimState)  where
    setRandom rs = sRandoms .= rs
    getRandom = use sRandoms      

collectData :: Simulation SimData
collectData =  do
    ap <- use avgPrice
    return [("avgPrice", log ap)]

mapMw :: (Worker -> Simulation Worker) -> Simulation ()
mapMw f = do
    ws <- use workers
    ws' <- traverse f ws
    workers .= ws'

mapMp :: (Producer -> Simulation Producer) -> Simulation ()
mapMp f = do
    ps <- use producers
    ps' <- traverse f ps
    producers .= ps'

mapMb :: (Bank -> Simulation Bank) -> Simulation ()
mapMb f = do
    bs <- use banks
    bs' <- traverse f bs
    banks .= bs'
    
