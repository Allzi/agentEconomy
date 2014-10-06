{-# LANGUAGE MultiWayIf #-}
module FBeginning 
( planStep
)where
import Prelude hiding (sum, foldl, any, mapM_)

import Control.Lens
import Control.Monad.State.Strict hiding (mapM_)
import Data.Foldable
import Data.Random
import Data.Random.Distribution.Bernoulli

import AgentTypes
import Simulation
import SimUtils


-- | In planStep all the firms prepare for next month.
-- Offered wage rates are adjusted up if there are unfilled positions,
-- and down if there are enought months of full staff.
-- Hiring and firing happens according to inventory and past demand.
-- If hiring or firing happens, prices are adjusted using a sort of mark-up pricing.
-- In the end the records are cleared for next month.
planStep :: Simulation ()
planStep = do
    firms %= fmap isFullStaff
    firms <$=> (adjustWage >=> fire >=> hire >=> return . clear)
  where
    clear f = f&fMDemand .~ 0
    isFullStaff f = if f^.fOpenPositions < 1
        then f&fFullStaffMonths +~ 1
        else f&fFullStaffMonths .~ 0

adjustWage :: Firm -> Simulation Firm
adjustWage f = do
    wage <- lift $ if 
        | f^.fOpenPositions > 0 -> 
            uniformAdj wageAdj (f^.fWageRate)
        | f^.fFullStaffMonths < rateDropWait -> return (f^.fWageRate)
        | otherwise -> uniformAdj (-wageAdj) (f^.fWageRate)
    return $ f&fWageRate .~ wage

fire :: Firm -> Simulation Firm
fire f = if (f^.fFiring) && (f^.fSize > 0)
    then do
        hid <- lift $ randomElementN (f^.fSize) (f^.fWorkers)
        households.ix hid %= (\h -> h&hEmployer .~ Nothing)
        return $ f&fFiring          .~ False
                  &fSize            -~ 1
                  &fWorkers         %~ filter (/=hid)
    else return $ f&fFiring .~ False

hire :: Firm -> Simulation Firm
hire f = lift $ if
    | f^.fInventory < iLowerBound * f^.fMDemand -> do
        let f' = f&fOpenPositions .~ 1
        risePrice f'
    | f^.fInventory > iUpperBound * f^.fMDemand ->
        if f^.fSize > 1
            then do
                let f' = f&fFiring .~ True
                dropPrice f'
            else dropPrice f
    | otherwise -> return f

-- | Price is risen only if hiring decision is made.
-- Rising the price happens with a certain probability, 
-- and with a random adjustment.
risePrice :: Firm -> RVar Firm
risePrice f = do
    doesRise <- bernoulli priceAdjProb
    if doesRise && (f^.fPrice < pLowerBound * mrc)
        then do
            newp <- uniformAdj priceAdj (f^.fPrice) 
            return $ f&fPrice .~ newp
        else return f
  where
    mrc = f^.fWageRate / (productivity * fromIntegral daysInMonth)

-- | Price drop happens only if firing decision is made.
-- Drop happens with a certain probability, and with a random adjustment.
dropPrice :: Firm -> RVar Firm
dropPrice f = do
    doesChange <- bernoulli priceAdjProb
    if doesChange && (f^.fPrice > pUpperBound * mrc)
        then do
            newp <- uniformAdj (-priceAdj) (f^.fPrice) 
            return $ f&fPrice .~ newp
        else return f
  where
    mrc = f^.fWageRate / (productivity * fromIntegral daysInMonth)




