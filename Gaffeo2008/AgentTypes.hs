{-# LANGUAGE TemplateHaskell #-}
module AgentTypes where
import Control.Lens
import qualified Data.IntMap.Strict as Map
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
    _wID :: !Wid,
    _wEmployer :: Maybe Pid,
    _wOffers :: [(Pid, Money)],
    _wIncome :: !Money,
    _wSavings :: !Money,
    _wShop :: Maybe Pid
}

data Producer = Producer {
    _pID :: !Pid,
    --Plans
    _pQuantity :: !Stuff,
    _pPrice :: !Money,
    _pAC :: !Money,
    --Labor
    _pWorkers :: [(Wid, Money)],
    _pApplicants :: [Wid],
    _pLDemand :: !Int,
    _pEntrantWage :: !Money,
    --Production
    _pProductivity :: !Double,
    _pGoods :: Maybe Stuff,
    --Financial
    _pCash :: !Money,
    _pClosing :: !Money,
    _pProfit :: !Money,
    _pNomSales :: !Money,
    _pDebt :: Maybe (Bid, Money)
}

data Bank = Bank {
    _bID :: !Bid,
    _bCash :: !Money,
    _bMarkUp :: Double -> Double,
    _bDebt :: !Money
}

makeLenses ''Worker
makeLenses ''Producer
makeLenses ''Bank

makeMapWith :: [Int] -> (Int -> a) -> Map.IntMap a
makeMapWith ids a = Map.fromList $ fmap (\i -> (i, a i)) ids

makeWorker :: Int -> Worker
makeWorker i = Worker {
    _wID = i,
    _wEmployer = Nothing,
    _wOffers = [],
    _wIncome = 0,
    _wSavings = 10,
    _wShop = Nothing
}


makeProducer :: Int -> Producer
makeProducer i = Producer {
    _pID = i,
    _pWorkers = [],
    _pApplicants = [],
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

makeBank :: Int -> Bank
makeBank i = Bank {
    _bID = i,
    _bCash = 10.0,
    _bMarkUp = (*0.01), -- check this!
    _bDebt = 0
}




class Firm a where
    isSolvent :: a -> Bool
    makeEntrant :: a -> a
    idLens :: Lens' a Int

instance Firm Producer where
    isSolvent a = isNothing (a^.pDebt)
    makeEntrant a = a&pWorkers .~ []
    idLens = pID

instance Firm Bank where
    isSolvent a = (a^.bCash) >= 0
    makeEntrant = id
    idLens = bID



