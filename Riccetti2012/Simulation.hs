{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Simulation where
import Control.Lens
import Control.Monad.State
import System.Random

import SimUtils
import AgentTypes
import Debug.Trace

-- Parameters:
seed, workerN, producerN, bankN, duration, consTrials, laborTrials, creditTrials :: Int
seed            = 1
workerN         = 500
producerN       = 80
bankN           = 10
consTrials      = 16
laborTrials     = 100
creditTrials    = 2
duration        = 1000

levAdj :: Double
levAdj = 0.05
inventoryTreshold = 0.1

type Simulation = State SimState

type SimData = [(String, Double)]

data SimState = SimState {
    _workers     :: WorkerMap,
    _workerIds   :: [Wid],
    _producers   :: ProducerMap,
    _producerIds :: [Pid],
    _banks       :: BankMap,
    _bankIds     :: [Bid],
    _sRandoms    :: [Double],
    _timer       :: Int
    }

makeLenses ''SimState

startSim = SimState {
    _workers = makeMapWith wids makeWorker,
    _workerIds = wids,
    _producers = makeMapWith pids makeProducer,
    _producerIds = pids,
    _banks = makeMapWith bids makeBank,
    _bankIds = bids,
    _sRandoms = rands,
    _timer = 0
    } 
  where 
    rands = randoms (mkStdGen seed)
    wids = [0..(workerN-1)]
    pids = [0..(producerN-1)] 
    bids = [0..(bankN-1)]

instance RSim (State SimState)  where
    setRandom rs = sRandoms .= rs
    getRandom = use sRandoms      

collectData :: Simulation SimData
collectData =  do
    return []

mapMw :: (Worker -> Simulation Worker) -> Simulation ()
mapMw f = do
    ws <- use workers
    ws' <- mapMOf traverse f ws
    workers .= ws'

mapMp :: (Producer -> Simulation Producer) -> Simulation ()
mapMp f = do
    ps <- use producers
    ps' <- mapMOf traverse f ps
    producers .= ps'


    
