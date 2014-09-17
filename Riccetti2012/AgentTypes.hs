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
type Stuff = Int


data Worker = Worker {
    _wID            :: Wid,
    _wWage          :: Money,
    _wEmployed      :: Bool,
    _wWealth        :: Money,
    _wDividends     :: Money,
    _wInterest      :: Money,
    _wConsBill      :: Money,
    _wDemand        :: Money
}

data Producer = Producer {
    _pID            :: Pid,
    --Finance
    _pNetWorth      :: Money,
    _pLiquity       :: Money,
    _pProfit        :: Money,
    _pNetProfit     :: Money,
    _pPayOutR       :: Double,
    _pIsSolvent     :: Bool,
    --Credit
    _pLevTarg       :: Double,
    _pDebtDemand    :: Money,
    _pInterest      :: Double,
    _pDebts         :: [(Bid, Money, Double)],
    _pDebt          :: Money,
    --Goods
    _pProduction    :: Stuff,
    _pInventory     :: Stuff,
    _pPrice         :: Money,
    _pSales         :: Money,
    --hiring
    _pWorkers       :: Int,
    _pWageBill      :: Money
}

data Bank = Bank {
    _bID            :: Bid,
    _bMaxCredit     :: Money,
    _bInterest      :: Double,
    _bUnlendedFunds :: Money,
    _bNetWorth      :: Money,
    _bCBCredit      :: Money,
    _bDeposits      :: Money,
    _bBadDebt       :: Money,
    _bDepRate       :: Double
}

data Government = Government {
    --Tax rates:
    _gIncomeTax     :: Double,
    _gWealthTax     :: Double,
    _gWTaxTreshold  :: Double,
    --Incomes and costs:
    _gTaxIncome     :: Money,
    _gWageBill      :: Money,
    _gRescueCosts   :: Money,
    _gIntToBanks    :: Money,
    --Financial:
    _gInterest      :: Double,
    _gPubDebt       :: Money,
    --Misc.:
    _gWorkerShare   :: Double
}

makeLenses ''Worker
makeLenses ''Producer
makeLenses ''Bank
makeLenses ''Government

makeMapWith :: [Int] -> (Int -> a) -> Map.IntMap a
makeMapWith ids a = Map.fromList $ fmap (\i -> (i, a i)) ids

makeWorker :: Wid -> Worker
makeWorker i = Worker {
    _wID        = i,
    _wWage      = 2,
    _wEmployed  = False,
    _wWealth    = 10,
    _wDividends = 0,
    _wInterest  = 0,
    _wConsBill  = 0,
    _wDemand    = 0
}


makeProducer :: Pid -> Producer
makeProducer i = Producer {
    _pID        = i,
    --Finance
    _pNetWorth  = 10,
    _pLiquity   = 10,
    _pProfit    = 0,
    _pNetProfit = 0,
    _pPayOutR   = 0.1,
    _pIsSolvent = True,
    --Credit
    _pLevTarg   = 1,
    _pDebtDemand = 0, -- derived from leverage target
    _pInterest  = 0.01,
    _pDebts     = [],
    _pDebt      = 0,
    --Goods
    _pProduction = 12,
    _pInventory = 0,
    _pPrice     = 1,
    _pSales     = 0,
    --hiring
    _pWorkers   = 0,
    _pWageBill  = 0
}

makeBank :: Bid -> Bank
makeBank i = Bank {
    _bID            = i,
    _bMaxCredit     = 0, --derived later
    _bUnlendedFunds = 0,
    _bInterest      = 0.02,
    _bNetWorth      = 10,
    _bCBCredit      = 1,
    _bDeposits      = 0,
    _bBadDebt       = 0,
    _bDepRate       = 0.01
}

makeGovernment :: Government
makeGovernment = Government {
    --Tax rates:
    _gIncomeTax     = 0.3,
    _gWealthTax     = 0.5,
    _gWTaxTreshold  = 3,
    --Incomes and costs:
    _gTaxIncome     = 0,
    _gWageBill      = 0,
    _gRescueCosts   = 0,
    _gIntToBanks    = 0,
    --Financial:
    _gInterest      = 0.01, 
    _gPubDebt       = 0,
    --Misc.:
    _gWorkerShare   = 0.33
}
