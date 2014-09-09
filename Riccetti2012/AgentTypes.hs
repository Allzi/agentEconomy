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
    _wID :: Wid
}

data Producer = Producer {
    _pID :: Pid,
    --Finance
    _pNetWorth :: Money,
    _pLevTarg :: Double,
    _pProfit :: Money,
    _pDebt  :: Money,
    --Goods
    _pProduction :: Stuff,
    _pInventory :: Stuff
}

data Bank = Bank {
    _bID :: Bid
}

makeLenses ''Worker
makeLenses ''Producer
makeLenses ''Bank

makeMapWith :: [Int] -> (Int -> a) -> Map.IntMap a
makeMapWith ids a = Map.fromList $ fmap (\i -> (i, a i)) ids

makeWorker i = Worker {
    _wID = i 
}



makeProducer i = Producer {
    _pID = i,
    --Finance
    _pNetWorth = 10,
    _pLevTarg = 2,
    _pProfit = 0,
    _pDebt = 0,
    --Goods
    _pProduction = 12,
    _pInventory = 10
}

makeBank i = Bank {
    _bID = i
}
