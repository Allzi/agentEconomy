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
    _wWageIncome    :: Money,
    _wEmployed      :: Bool,
    _wWealth        :: Money,
    _wDividends     :: Money,
    _wMinInterest   :: Double,
    _wInterest      :: Money,
    _wConsBill      :: Money,
    _wDemand        :: Money,
    _wDepSupply     :: Money
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
    -- Financial:
    _bNetWorth      :: Money,
    _bProfit        :: Money,
    _bNetProfit     :: Money,
    _bPayOutR       :: Double,
    _bIsSolvent     :: Bool,
    -- Incomes and costs:
    _bLendIncome    :: Money,
    _bBadDebt       :: Money,
    _bDepCost       :: Money,
    _bCBCredCost    :: Money,
    _bPDIncome      :: Money,
    -- Lending out:
    _bMaxCredit     :: Money,
    _bInterest      :: Double,
    _bUnlendedFunds :: Money,
    _bPDDemand      :: Money,
    _bPD            :: Money,
    -- Fund sources:
    _bCBCredit      :: Money,
    _bDeposits      :: Money,
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
    _wWageIncome = 0,
    _wWealth    = 10,
    _wDividends = 0,
    _wMinInterest = 0.01,
    _wInterest  = 0,
    _wConsBill  = 0,
    _wDemand    = 0,
    _wDepSupply = 0
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
    -- Financial:
    _bNetWorth      = 10,
    _bProfit        = 0,
    _bNetProfit     = 0,
    _bPayOutR       = 0.1,
    _bIsSolvent     = True,
    -- Incomes and costs:
    _bLendIncome    = 0,
    _bBadDebt       = 0,
    _bDepCost       = 0,
    _bCBCredCost    = 0,
    _bPDIncome      = 0,
    -- Lending out:
    _bMaxCredit     = 0,
    _bInterest      = 0.01,
    _bUnlendedFunds = 0,
    _bPDDemand      = 0,
    _bPD            = 0,
    -- Fund sources:
    _bCBCredit      = 1,
    _bDeposits      = 0,
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
