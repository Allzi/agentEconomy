{-# LANGUAGE MultiWayIf #-}
module HBeginning
( sellerSearch
, jobSearch
, consumptionPlans
) where
import Prelude hiding (sum, foldl, any, mapM_)

import Control.Lens
import Control.Monad.State.Strict hiding (mapM_)
import qualified Data.IntMap as Map
import Data.Foldable
import Data.Function
import Data.List hiding (sum, foldl, any)
import Data.Random
import Data.Random.Distribution.Bernoulli

import AgentTypes
import Simulation
import SimUtils

-- | In sellerSearch households replace expensive and unreliable links.
sellerSearch :: Simulation ()
sellerSearch = do
    fs <- use firms
    let sizedFids = fmap getFirmWeight fs
        wSum = sum sizedFids
    households <$=> updateShops (getRandFid sizedFids wSum)
  where
    getRandFid :: Map.IntMap Double -> Double -> Household -> RVar Fid
    getRandFid sfs weightSum  h = do
        let (sfs', ws) = foldl folder (sfs, weightSum) (h^.hShops)
        (fid, _) <- whRandElem (Map.toList sfs') snd ws
        return fid
      where
        folder (s, w) (fid, _) = (Map.delete fid s, w - weight)
          where
            Just weight = Map.lookup fid s
    getFirmWeight :: Firm -> Double
    getFirmWeight f = fromIntegral (f^.fSize + f^.fOpenPositions)

updateShops :: (Household -> RVar Fid) -> Household -> Simulation Household
updateShops getRandFid hous = updatePrices hous >>= pSearch >>= qSearch
  where
    updatePrices :: Household -> Simulation Household
    updatePrices h = do
        nShops <- traverse updatePrice (h^.hShops)
        return $ h&hShops .~ nShops
      where
        updatePrice :: (Fid, Money) -> Simulation (Fid, Money)
        updatePrice (fid, _) = do
            Just f <- use $ firms.at fid
            return (fid, f^.fPrice)

    pSearch :: Household -> Simulation Household
    pSearch h = do
        s <- lift $ bernoulli pSearchProb
        if s
            then do
                (fid1, p1) <- lift $ randomElementN shopN (h^.hShops)
                fid2       <- lift $ getRandFid h
                Just f2    <- use $ firms.at fid2
                if p1 > f2^.fPrice * (1 - diffToReplace)
                    then return $ 
                        h&hShops %~ replaceShop fid1 (fid2, f2^.fPrice)
                         &hUnsatDemand %~ filter (\a -> fst a /= fid1)
                    else return h
            else return h

    qSearch :: Household -> Simulation Household
    qSearch h = do
        s <- lift $ bernoulli qSearchProb
        if s && (not.null) (h^.hUnsatDemand)
            then do
                let totWeight = (sum.fmap snd) (h^.hUnsatDemand)
                (toReplace, _)  <-
                    lift $ whRandElem (h^.hUnsatDemand) snd totWeight
                newS            <- lift $ getRandFid h
                Just f          <- use $ firms.at newS
                let h' = h&hShops %~ replaceShop toReplace (newS, f^.fPrice)
                          &hUnsatDemand .~ []
                return h'
            else return h

    replaceShop :: Fid -> (Fid, Money) -> [(Fid, Money)] -> [(Fid, Money)]
    replaceShop fid nShop shops = nShop:filt
      where
        filt = filter (( /= fid) . fst) shops
        

-- | Job search is done in random order.
jobSearch :: Simulation ()
jobSearch = do
    hids <- use householdIds
    shuffled <- lift $ shuffleN householdN hids
    mapM_ search shuffled
  where
    search hid = do
        Just h <- use $ households . at hid
        h' <- searchJ h
        households.ix hid .= h'

-- | Household can search jobs with three different intensities.
-- Unemployed worker searches multiple firms for a hiring firm with greater wage 
-- rate than his reservation wage.
-- Unsatisfied and satisfied workers try to find a better employer with different
-- intensities.
searchJ :: Household -> Simulation Household
searchJ h = case h^.hEmployer of
    Nothing -> searchGoodEnough unempVisits
    Just fid -> do
        let Just w = h^.hWage
        Just f <- use $ firms.at fid
        doSearch <- lift $ if w < h^.hResWage
            then bernoulli unsatProb
            else bernoulli satProb
        if doSearch 
            then searchBetter f
            else return h
  where 
    searchGoodEnough :: Int -> Simulation Household
    searchGoodEnough 0 = return h
    searchGoodEnough i = do
        fids <- use firmIds
        fid <- lift $ randomElement fids
        Just f <- use $ firms.at fid
        if (f^.fWageRate > h^.hResWage) && (f^.fOpenPositions > 0)
            then do
                let f' = hireWorker (h^.hID) f
                firms.ix fid .= f'
                return $ h&hEmployer .~ Just fid
            else searchGoodEnough (i-1)

    searchBetter :: Firm -> Simulation Household
    searchBetter f = do
        fids <- use firmIds
        rFid <- lift $ randomElement (filter (/=(f^.fID)) fids)
        Just f2 <- use $ firms.at rFid
        if (f2^.fOpenPositions > 0) && (f2^.fWageRate > f^.fWageRate)
            then do
                let f' = loseWorker (h^.hID) f
                firms.ix (f'^.fID) .= f'
                let f2' = hireWorker (h^.hID) f2
                firms.ix (f2'^.fID) .= f2'
                let h' = h&hEmployer .~ Just (f2^.fID)
                return h'
            else return h

-- | Households plan consumption for incoming month according to their 
-- real wealth.
-- Daily demand is monthly / 21.
consumptionPlans :: Simulation ()
consumptionPlans = households %= fmap planConsumption
  where
    planConsumption :: Household -> Household
    planConsumption h = h&hDDemand .~ (demand / fromIntegral daysInMonth)
      where
        avgP = (sum.fmap snd) (h^.hShops) / fromIntegral shopN
        realWealth = h^.hLiquity / avgP
        demand = min (realWealth ** consAlpha) realWealth
        

-- | What happens in a firm when an employee quits.
loseWorker :: Hid -> Firm -> Firm
loseWorker hid f = f&fWorkers       %~ filter (/=hid)
                    &fOpenPositions +~ 1
                    &fSize          -~ 1

-- | What happens in a firm when an employee is succesfully hired.
hireWorker :: Hid -> Firm -> Firm
hireWorker hid f = f&fWorkers       %~ (hid:)
                    &fOpenPositions -~ 1
                    &fSize          +~ 1
    


