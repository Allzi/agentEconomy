{-# LANGUAGE TemplateHaskell #-}
module AgentTypes where
import Control.Lens
import qualified Data.IntMap as Map
import Data.Maybe

type Wid = Int
type WorkerMap = Map.IntMap Worker
type Pid = Int
type ProducerMap = Map.IntMap Producer
type Bid = Int
type BankMap = Map.IntMap Bank
type Money = Double
type Stuff = Double


data Worker = Worker {
    _wID            :: Wid,
    _wWage          :: Money,
    _wEmployed      :: Bool
}

data Producer = Producer {
    _pID            :: Pid,
    --Finance
    _pNetWorth      :: Money,
    _pLevTarg       :: Double,
    _pDebtDemand    :: Money,
    _pInterest      :: Double,
    _pProfit        :: Money,
    _pDebts         :: [(Bid, Money, Double)],
    _pDebt          :: Money,
    --Goods
    _pProduction    :: Stuff,
    _pInventory     :: Stuff
}

data Bank = Bank {
    _bID            :: Bid,
    _bMaxCredit     :: Money,
    _bInterest      :: Double,
    _bUnlendedFunds :: Money,
    _bNetWorth      :: Money,
    _bDebt          :: Money,
    _bCBCredit      :: Money,
    _bDeposits      :: Money
}

data Goverment = Goverment {
    _gIncomeTax     :: Double,
    _gWealthTax     :: Double,
    _gWTaxTreshold  :: Money,
    _pWorkerShare   :: Double
       
}

makeLenses ''Worker
makeLenses ''Producer
makeLenses ''Bank
makeLenses ''Goverment

makeMapWith :: [Int] -> (Int -> a) -> Map.IntMap a
makeMapWith ids a = Map.fromList $ fmap (\i -> (i, a i)) ids

makeWorker :: Wid -> Worker
makeWorker i = Worker {
    _wID = i,
    _wWage = 2,
    _wEmployed = False    
}


makeProducer :: Pid -> Producer
makeProducer i = Producer {
    _pID = i,
    --Finance
    _pNetWorth = 10,
    _pLevTarg = 1,
    _pDebtDemand = 0, -- derived from leverage target
    _pInterest = 0.01,
    _pProfit = 0,
    _pDebts = [],
    _pDebt = 0,
    --Goods
    _pProduction = 12,
    _pInventory = 10
}

makeBank :: Bid -> Bank
makeBank i = Bank {
    _bID = i,
    _bMaxCredit = 0, --derived later
    _bUnlendedFunds = 0,
    _bInterest = 0.01,
    _bNetWorth = 10,
    _bDebt = 0,
    _bCBCredit = 1,
    _bDeposits = 0
}
