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
    _hID        :: !Hid,
    _hLiquity   :: !Money
}

data Firm = Firm {
    _fID                :: !Fid,
    _fLiquity           :: !Money,
    _fPrice             :: !Money,
    -- Workers
    _fWorkers           :: [Hid],
    _fFiring            :: Maybe Hid, --Worker to be fired after a month
    _fWageRate          :: !Money,
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
    _hID        = i,
    _hLiquity   = undefined
}


makeFirm :: Fid -> Firm
makeFirm i = Firm {
    _fID                = i,
    _fLiquity           = undefined,
    _fPrice             = 1,
    -- Workers
    _fWorkers           = [],
    _fFiring            = Nothing,
    _fWageRate          = 1,
    _fOpenPositions     = 0,
    _fFullStaffMonths   = 0,
    -- Stuff
    _fInventory         = 0,
    _fMDemand           = 1
}

