{-# LANGUAGE MultiWayIf #-}
module Month where
import Prelude hiding (sum, foldl, any, mapM_)

import Control.Lens
import Control.Monad.State.Strict hiding (mapM_)
import qualified Data.IntMap as Map
import Data.Foldable
import Data.Function
import Data.List hiding (sum, foldl, any)
import Control.Applicative
import Debug.Trace
import System.Random
import Data.Random
import Data.Random.Distribution.Bernoulli

import AgentTypes
import Simulation
import SimUtils
import Day

runMonths :: Simulation ()
runMonths = do
    timer._2 .~ 0
    runMonth

-- | Advances simulation by one month.
-- TODO: add actual firing of employee
runMonth :: Simulation ()
runMonth = do
    planStep
    sellerSearch
    jobSearch
    consumptionPlans
    
    runDays    
    
    -- End of month:
    -- accounting (wages, dividents)
    -- adjustReservation
    timer._2 +~ 1
    m <- use $ timer._2

    while (m < 12) runMonth

-- | In planStep all the firms prepare for next month.
-- Offered wage rates are adjusted up if there are unfilled positions,
-- and down if there are enought months of full staff.
-- Hiring and firing happens according to inventory and past demand.
-- If hiring or firing happens, prices are adjusted using a sort of mark-up pricing.
-- In the end the records are cleared for next month.
planStep :: Simulation ()
planStep = firms <$=> (adjustWage >=> fire >=> hire >=> return . clear)
  where
    adjustWage :: Firm -> Simulation Firm
    adjustWage f = do
        wage <- lift $ if 
            | f^.fOpenPositions /= 0 -> 
                uniformAdj wageAdj (f^.fWageRate)
            | f^.fFullStaffMonths < rateDropWait -> return (f^.fWageRate)
            | otherwise -> uniformAdj (-wageAdj) (f^.fWageRate)
        return $ f&fWageRate .~ wage
    fire :: Firm -> Simulation Firm
    fire f = case f^.fFiring of
        Nothing -> return f
        Just hid -> if any (==hid) (f^.fWorkers)
            then do
                households.ix hid %= (\h -> h&hEmployer .~ Nothing)
                return $ f&fFiring          .~ Nothing
                          &fOpenPositions   +~ 1
                          &fSize            -~ 1
                          &fWorkers         %~ filter (/=hid)
            else return $ f&fFiring .~ Nothing
    hire :: Firm -> Simulation Firm
    hire f = lift $ if
        | f^.fInventory > iUpperBound * f^.fMDemand -> do
            let f' = f&fOpenPositions +~ 1
            updatePrice f'
        | f^.fInventory < iLowerBound * f^.fMDemand -> do
            fired <- randomElement (f^.fWorkers)
            let f' = f&fOpenPositions -~ 1
                      &fFiring .~ Just fired
            updatePrice f'
        | otherwise -> return f
    -- Price is updated if hiring/firing decision is made.
    updatePrice :: Firm -> RVar Firm
    updatePrice f = if
        | f^.fPrice * productivity > pUpperBound * f^.fWageRate -> do
            newP <- uniformAdj (-priceAdj) (f^.fPrice) 
            return $ f&fPrice .~ newP
        | f^.fPrice * productivity < pLowerBound * f^.fWageRate -> do
            newP <- uniformAdj priceAdj (f^.fPrice) 
            return $ f&fPrice .~ newP
        | otherwise -> return f
    clear :: Firm -> Firm
    clear f = f&fMDemand .~ 0


-- | In sellerSearch households replace expensive and unreliable links.
sellerSearch :: Simulation ()
sellerSearch = do
    fs <- use firms
    let sizedFids = fmap (\f -> fromIntegral (f^.fSize)) fs
        wSum = sum sizedFids
    households <$=> updateLinks sizedFids wSum
    households %= fmap sortShops
  where  
    updateLinks :: Map.IntMap Double -> Double -> Household -> Simulation Household
    updateLinks sfs weightSum hous = updatePrices hous >>= pSearch >>= qSearch
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
                    (fid1, p1) <- lift $ randomElement (h^.hShops)
                    fid2 <- lift $ getRandFid h
                    Just f2 <- use $ firms.at fid2
                    if p1 > f2^.fPrice * (1 - diffToReplace)
                        then do
                            let h' = h&hShops %~ replaceShop fid1 (fid2, f2^.fPrice)
                                      &hUnsatDemand %~ filter (\a -> fst a /= fid1)
                            return h'
                        else return h
                else return h
        qSearch :: Household -> Simulation Household
        qSearch h = do
            s <- lift $ bernoulli qSearchProb
            if s && (not.null) (h^.hUnsatDemand)
                then do
                    let totWeight = (sum.fmap snd) (h^.hUnsatDemand)
                    (toReplace, _) <- lift $ whRandElem (h^.hUnsatDemand) snd totWeight
                    newS <- lift $ getRandFid h
                    Just f <- use $ firms.at newS
                    let h' = h&hShops %~ replaceShop toReplace (newS, f^.fPrice)
                              &hUnsatDemand .~ []
                    return h'
                else return h
        getRandFid :: Household -> RVar Fid
        getRandFid h = do
            let (sfs', weightAdj) = foldl folder (sfs, 0) (h^.hShops)
            (fid, _) <- whRandElem (Map.toList sfs') snd (weightSum - weightAdj)
            return fid
          where
            folder (s, w) (fid, _) = (Map.delete fid s, w + weight)
              where
                Just weight = Map.lookup fid s
        replaceShop :: Fid -> (Fid, Money) -> [(Fid, Money)] -> [(Fid, Money)]
        replaceShop fid nShop = traverse.filtered (( == fid) . fst) .~ nShop
    sortShops :: Household -> Household
    sortShops h = h&hShops %~ sortBy (compare `on` snd)


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
        Just f <- use $ firms.at fid
        if f^.fWageRate < h^.hResWage
            then do
                doSearch <- lift $ bernoulli unsatProb
                if doSearch 
                    then searchBetter f
                    else return h
            else do
                doSearch <- lift $ bernoulli satProb
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
    

-- * Utilities

-- | An utility function, that adjusts uniformly the double given as the second
-- argument. The first argument tells the maximum relative adjustment.
-- Giving negative maximum adjustment makes the adjustment negative.
uniformAdj :: Double -> Double -> RVar Double
uniformAdj adj a = (\r -> a * (1 + adj * r)) <$> stdUniform

-- | Gets random element from a list with given weights.
whRandElem :: [a] -> (a -> Double) -> Double -> RVar a
whRandElem ls weight m = do
    r <- uniform 0 m
    fetchElem r ls weight
  where
    fetchElem r (l:ls) weight = do
        let nr = r - weight l
        if nr < 0
            then return l
            else fetchElem nr ls weight
    fetchElem r [] weight = error "Empty list or too big sum of weights!"

