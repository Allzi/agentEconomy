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
    _wEmployer :: Maybe Pid,
    _wIncome :: Money,
    _wSavings :: Money,
    _wShop :: Maybe Pid
}

data Producer = Producer {
    --Plans
    _pQuantity :: Stuff,
    _pPrice :: Money,
    _pAC :: Money,
    --Labor
    _pWorkers :: [(Wid, Money)],
    _pLDemand :: Int,
    _pNAppls :: [Wid],
    _pEntrantWage :: Money,
    --Production
    _pProductivity :: Double,
    _pGoods :: Maybe Stuff,
    --Financial
    _pCash :: Money,
    _pClosing :: Money,
    _pProfit :: Money,
    _pNomSales :: Money,
    _pDebt :: Maybe (Bid, Money)
}

data Bank = Bank {
    _bCash :: Money,
    _bMarkUp :: Double -> Double,
    _bDebt :: Money
}

makeLenses ''Worker
makeLenses ''Producer
makeLenses ''Bank

makeMapWith :: [Int] -> a -> Map.IntMap a
makeMapWith ids a = Map.fromList $ fmap (\i -> (i, a)) ids

makeWorker = Worker {
    _wEmployer = Nothing,
    _wIncome = 0,
    _wSavings = 10,
    _wShop = Nothing
}



makeProducer = Producer {
    _pWorkers = [],
    _pNAppls = [],
    _pPrice = 1.0,
    _pProductivity = 1.0,
    _pQuantity = 5.0,
    _pProfit = 0,
    _pNomSales = 1.0,
    _pGoods = Nothing,
    _pCash = 10.0,
    _pClosing = 10.0,
    _pDebt = Nothing,
    _pEntrantWage = 1,
    _pLDemand = 0,
    _pAC = 1
}

makeBank = Bank {
    _bCash = 10.0,
    _bMarkUp = (*0.1), -- check this!
    _bDebt = 0
}




class Firm a where
    isSolvent :: a -> Bool
    makeEntrant :: a -> a

instance Firm Producer where
    isSolvent a = isNothing (a^.pDebt)
    makeEntrant a = a&pWorkers .~ [] 

instance Firm Bank where
    isSolvent a = (a^.bCash) >= 0
    makeEntrant = id



