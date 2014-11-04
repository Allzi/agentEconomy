{-# LANGUAGE TemplateHaskell #-}
module AgentTypes where
import Control.Lens
import qualified Data.IntMap.Strict as Map

type Hid = Int
type HMap = Map.IntMap Household
type Fid = Int
type FMap = Map.IntMap Firm
type Money = Double
type Stuff = Double


data Household = Household {
    _hID            :: !Hid,
    _hLiquity       :: !Money,
    _hShops         :: ![Fid],
    _hUnsatDemand   :: ![(Fid, Money)],
    _hDDemand       :: !Stuff,
    _hEmployer      :: Maybe Fid,
    _hResWage       :: !Money
}

data Firm = Firm {
    _fID                :: !Fid,
    _fLiquity           :: !Money,
    _fPrice             :: !Money,
    -- Workers
    _fWorkers           :: [Hid],
    _fSize              :: !Int,
    _fFiring            :: !Bool,
    _fWageRate          :: !Money,
    _fSizeTarget        :: !Int,
    _fFullStaffMonths   :: !Int,
    -- Stuff
    _fInventory         :: !Stuff,
    _fMDemand           :: !Stuff --Sum of sales during a month
}


makeLenses ''Household
makeLenses ''Firm

makeMapWith :: [Int] -> (Int -> a) -> Map.IntMap a
makeMapWith ids a = Map.fromList $ fmap (\i -> (i, a i)) ids

makeHousehold :: Hid -> Household
makeHousehold i = Household {
    _hID            = i,
    _hLiquity       = 98.78,
    _hShops         = [],
    _hUnsatDemand   = [],
    _hDDemand       = 0, --determined later
    _hEmployer      = Nothing,
    _hResWage       = 0
}


makeFirm :: Fid -> Firm
makeFirm i = Firm {
    _fID                = i,
    _fLiquity           = 0,
    _fPrice             = 1,
    -- Workers
    _fWorkers           = [],
    _fSize              = 0,
    _fFiring            = False,
    _fWageRate          = 52,
    _fSizeTarget        = 0,
    _fFullStaffMonths   = 0,
    -- Stuff
    _fInventory         = 50,
    _fMDemand           = 0
}

