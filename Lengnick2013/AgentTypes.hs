{-# LANGUAGE TemplateHaskell #-}
module AgentTypes where
import Control.Lens
import qualified Data.IntMap as Map

type Hid = Int
type HMap = Map.IntMap Household
type Fid = Int
type FMap = Map.IntMap Firm
type Money = Double
type Stuff = Double


data Household = Household {
    _hID            :: !Hid,
    _hLiquity       :: !Money,
    _hShops         :: [(Fid, Money)],
    _hUnsatDemand   :: [(Fid, Money)],
    _hDDemand       :: Stuff,
    _hEmployer      :: Maybe Fid,
    _hWage          :: Maybe Money, --wage actually got
    _hResWage       :: Money
}

data Firm = Firm {
    _fID                :: !Fid,
    _fLiquity           :: !Money,
    _fPrice             :: !Money,
    -- Workers
    _fWorkers           :: [Hid],
    _fSize              :: Int,
    _fFiring            :: Maybe Hid, --Worker to be fired after a month
    _fWageRate          :: !Money,
    _fActualWage        :: !Money,
    _fOpenPositions     :: !Int,
    _fFullStaffMonths   :: Int,
    -- Stuff
    _fInventory         :: !Stuff,
    _fMDemand           :: Stuff --Sum of sales during a month
}


makeLenses ''Household
makeLenses ''Firm

makeMapWith :: [Int] -> (Int -> a) -> Map.IntMap a
makeMapWith ids a = Map.fromList $ fmap (\i -> (i, a i)) ids

makeHousehold :: Hid -> Household
makeHousehold i = Household {
    _hID            = i,
    _hLiquity       = 10,
    _hShops         = undefined,
    _hUnsatDemand   = [],
    _hDDemand       = 0, --determined later
    _hEmployer      = Nothing,
    _hWage          = Nothing,
    _hResWage       = 2
}


makeFirm :: Fid -> Firm
makeFirm i = Firm {
    _fID                = i,
    _fLiquity           = 10,
    _fPrice             = 1.2,
    -- Workers
    _fWorkers           = [],
    _fSize              = 0,
    _fFiring            = Nothing,
    _fWageRate          = 3,
    _fActualWage        = 3,
    _fOpenPositions     = 11,
    _fFullStaffMonths   = 0,
    -- Stuff
    _fInventory         = 0,
    _fMDemand           = 0
}

