{-# LANGUAGE TemplateHaskell #-}
module Simulation where
import Control.Lens
import Control.Monad.State
import System.Random

import AgentTypes
import Debug.Trace

-- Parameters:
seed            = 1     :: Int
workerN         = 500   :: Int
producerN       = 100   :: Int
bankN           = 10    :: Int
duration        = 1000    :: Int
mrC1            = 0.5   :: Double-- when income
mrC2            = 1.0   :: Double-- when no income
rdinv1          = 0.1   :: Double-- for rich
rdinv2          = 0     :: Double-- for poor
wageGrowth      = 0.05  :: Double
priceGrowth     = 0.1   :: Double
quantityGrowth  = 0.1   :: Double
bankCosts       = 0.1   :: Double
consTrials      = 2     :: Int
laborTrials     = 4     :: Int
creditTrials    = 2     :: Int

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
    _priceLevel  :: Money,
    _avgPrice    :: Money,
    _timer       :: Int,
    _iRate       :: Double,
    _aRSales     :: Double,
    _aNSales     :: Double
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
    _priceLevel = 1.0,
    _avgPrice = 1.0,
    _timer = 0,
    _iRate = 0.02,
    _aRSales = 0,
    _aNSales = 0
    } 
  where 
    rands = randoms (mkStdGen seed)
    wids = [0..(workerN-1)]
    pids = [0..(producerN-1)] 
    bids = [0..(bankN-1)]

collectData :: Simulation SimData
collectData =  do    
    pl <- use priceLevel
    rs <- use aRSales
    return [("log_price_level", log pl),
           ("price_level", pl),
           ("gdp", rs)]


