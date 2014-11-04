{-# LANGUAGE TemplateHaskell, FlexibleInstances, RankNTypes, GADTs #-}
module Simulation where
import Control.Lens
import Control.Monad.State.Strict
import Data.Random.Source
import Unsafe.Coerce
import Random.Xorshift

import Prelude hiding (foldl, maximum, minimum)
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as M

import AgentTypes
import SimUtils
import Control.DeepSeq



-- * Parameters
rateDropWait, householdN, firmN, duration, burnIn, unempVisits, 
    daysInMonth, shopN :: Int
-- | The number of households, default is 1000.
householdN      = 1000
-- | The number of firms, default is 100.
firmN           = 100
-- | The length of the simulation.
duration        = 250
-- | Burn-in duration, of which data is not collected.
burnIn          = 200
-- | Months of full staff before the firm starts to lower its wage. 
-- Default is 1. (There is a mistake in the article)
rateDropWait    = 1
-- | The number of firms that an unemployed visits when searching for a job.
-- Default is 5.
unempVisits     = 5
-- | How many (working) days are in a month. Default is 21.
daysInMonth     = 21
-- | How many local shops (type A connections) a household has. Default is 7.
shopN           = 7

-- | MAGIC
wageAdj, priceAdj, priceAdjProb, resWageDrop, qSearchProb, pSearchProb, 
    diffToReplace,iLowerBound, iUpperBound, pLowerBound, pUpperBound, 
    productivity, unsatProb, satProb, consAlpha, endShopTresh,
    mBufferMult  :: Double
-- | Maximum relative size of a wage adjustment. Default is 0.019.
wageAdj         = 0.02
-- | Maximum relative size of a price adjustment. Default is 0.02.
priceAdj        = 0.019
-- | Probability to adjust price at all. Default is 0.75.
priceAdjProb    = 0.78
-- | How much reservation wage drops after a month of unemployment.
-- Default is 0.1.
resWageDrop     = 0.1
-- | Probability for households to replace a shop wich could not provide
-- enough goods last month. Default is 0.25.
qSearchProb     = 0.25
-- | Probability for households to search for cheaper shops.
-- Default is 0.25.
pSearchProb     = 0.25
-- | Difference in price needed for replacement. Default is 0.01.
diffToReplace   = 0.01
-- | Limit of size of inventory relative to demand, below which a new worker
-- is hired. Default is 0.25.
iLowerBound     = 0.25
-- | Upper limit, above which a worker is fired. Default is 1.
iUpperBound     = 1
-- | How much above marginal cost a price is lowered. Default is 1.15.
pUpperBound     = 1.15
-- | How much below marginal cost a price is raised. Default is 1.025.
pLowerBound     = 1.025

productivity    = 3
-- | Probability that unsatisfied worker searches for a better job. 
-- Default is 1.
unsatProb       = 1
-- | Probability that satisfied worker searces for a better job. 
-- Deafault is 0.1.
satProb         = 0.1
-- | A parameter to determine households monthly demand as a function of its
-- monthly demand. Default is 0.9.
consAlpha       = 0.885
-- | Amount of satisfied demand needed before household ends shopping.
-- Default is 0.95.
-- Shopping can also end because there is no longer shops left.
endShopTresh    = 0.95
mBufferMult     = 0.1

-- * State

-- |Shorter name for our state.
-- Under the state monad is RVar from random-fu as a handy source of randomness.
type Simulation = StateT SimState Identity

-- |The state of the simulation.
-- Holds the agents, households and firms, with their ids.
data SimState = SimState {
    _households     :: !HMap,
    _householdIds   :: V.Vector Hid,
    _firms          :: !FMap,
    _firmIds        :: V.Vector Fid,
    _sDividends     :: !Money,
    _sHousWealth    :: !Money,
    _timer          :: !(Int, Int, Int),
    _sData          :: SimData,
    _sRGen          :: Xorshift
    }

makeLenses ''SimState

-- |'startSim' is the starting state of simulation.
startSim :: Int -> SimState
startSim s = SimState {
    _households     = makeMapWith hids makeHousehold,
    _householdIds   = V.fromList hids,
    _firms          = makeMapWith fids makeFirm,
    _firmIds        = V.fromList fids,
    _sDividends     = 0,
    _sHousWealth    = 0,
    _timer          = (0,0,0),
    _sData          = M.empty,
    _sRGen          = gen
    } 
  where
    hids = [0..(householdN-1)]
    fids = [0..(firmN-1)]
    gen = makeXorshift s



$(monadRandom [d|
        instance MonadRandom Simulation where
            getRandomWord64 = do
                gen <- use sRGen
                let (r, gen') = next gen
                sRGen .= gen'
                return $ unsafeCoerce r -- hehee
    |])


-- | All the data is collected here.
collectData :: Simulation ()
collectData = do
    t <- use $ timer._1
    hw <- use sHousWealth
    divid <- use sDividends
    hs <- use households
    fs <- use firms
    let -- Price levels:
        p = (foldl (\acc f -> acc + f^.fPrice) 0 fs) / fromIntegral firmN
        --maxPrice = (maximum . fmap (\f -> f^.fPrice)) fs
        --minPrice = (minimum . fmap (\f -> f^.fPrice)) fs
    
        -- Employment and wage:
        une = foldl calcUne 0 hs
        opos = foldl (\acc f -> acc + f^.fSizeTarget - f^.fSize) 0 fs
        
        -- Demand met:
        dem = (foldl (\acc f -> acc + f^.fMDemand) 0 fs) / fromIntegral firmN
        
        -- Offered and accepted wages:
        ow = (foldl (\acc f -> acc + f^.fWageRate) 0 fs) / fromIntegral firmN
        aw = (foldl (\acc h -> acc + h^.hResWage) 0 hs) / fromIntegral householdN
        
        -- Inventories:
        invs = foldl (\acc f -> acc + f^.fInventory) 0 fs / fromIntegral firmN

        --Sizes
        maxSize = (maximum . fmap (\f -> fromIntegral (f^.fSize))) fs
        minSize = (minimum . fmap (\f -> fromIntegral (f^.fSize))) fs

        maxWealth = (maximum . fmap (\h -> h^.hLiquity)) hs
        
        --Random firms
        Just f1 = fs^.at 97
        
        mrc f = f^.fWageRate / (productivity * fromIntegral daysInMonth)

        f1p = f1^.fPrice
        f1pl = mrc f1 * pLowerBound
        f1pu = mrc f1 * pUpperBound
        f1i = f1^.fInventory
        f1il = f1^.fMDemand * iLowerBound
        f1iu = f1^.fMDemand * iUpperBound
        f1s = fromIntegral (f1^.fSize)
        f1st = fromIntegral (f1^.fSizeTarget)
        f1w = f1^.fWageRate
        f1md = f1^.fMDemand
        f1liq = f1^.fLiquity

    when (t > burnIn) $ do
        seqAdd "Prices" 
            [("Pricelevel", p)
            ,("Random", f1p)
            ,("LowerBound", f1pl)
            ,("UpperBound", f1pu)]
        seqAdd "Inventories"
            [("Inventories",   invs)
            ,("Random", f1i)
            ,("Rand_LBound", f1il)
            ,("Rand_UBound", f1iu)]
        seqAdd "Wages"
            [("Offered_Wage",   ow)
            ,("Accepted_Wage",  aw)
            ,("Random", f1w)]
        seqAdd "Seen_Demand"
            [("Seen_Demand",   dem)
            ,("Random", f1md)]
        seqAdd "Wealth"
            [("Household_Wealh",     hw)
            ,("Aggregate_Dividends", divid)]
        seqAdd "Sizes"
            [("maxSize",  maxSize)
            ,("minSize",  minSize)
            ,("Random", f1s)
            ,("Target", f1st)]
        seqAdd "Employment"     [("UnemployedN",    une)]
        seqAdd "OpenPositions"  [("Open_Positions", fromIntegral opos)]
        seqAdd "Liquity"        [("RFirm", f1liq)]
        seqAdd "Time"           [("Time", fromIntegral t)]
        seqAdd "Richest"        [("Richest", maxWealth)]
        seqAdd "Margin"         [("Margin", f1p - mrc f1)]

            
  where
    seqAdd t d = d `deepseq` sData %= dataToTable t d
    calcUne acc h = if h^.hEmployer == Nothing
        then acc + 1
        else acc



