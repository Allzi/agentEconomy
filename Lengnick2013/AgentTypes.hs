{-# LANGUAGE TemplateHaskell #-}
module AgentTypes where
import Control.Lens
import qualified Data.IntMap as Map

type Hid = Int
type HMap = Map.IntMap Household
type Fid = Int
type FMap = Map.IntMap Firm
type Money = Double
type Stuff = Int


data Household = Household {
    _hID :: !Hid
}

data Firm = Firm {
    _fID :: !Fid
}


makeLenses ''Household
makeLenses ''Firm

makeMapWith :: [Int] -> (Int -> a) -> Map.IntMap a
makeMapWith ids a = Map.fromList $ fmap (\i -> (i, a i)) ids

makeHousehold :: Hid -> Household
makeHousehold i = Household {
    _hID = i
}


makeFirm :: Fid -> Firm
makeFirm i = Firm {
    _fID = i
}

