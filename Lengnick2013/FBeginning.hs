{-# LANGUAGE MultiWayIf #-}
module FBeginning 
( planStep
)where
import Prelude hiding (sum, foldl, any, mapM_)

import Control.Lens
import Control.Monad.State.Strict hiding (mapM_)
import Data.Foldable
import Data.Random

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
                      &fOpenPositions   .~ 1
                      &fSize            -~ 1
                      &fWorkers         %~ filter (/=hid)
        else return $ f&fFiring .~ Nothing


hire :: Firm -> Simulation Firm
hire f = lift $ if
    | f^.fInventory < iLowerBound * f^.fMDemand -> do
        let f' = f&fOpenPositions +~ 1
        updatePrice f'
    | f^.fInventory > iUpperBound * f^.fMDemand ->
        if f^.fSize > 1
            then do
                fired <- randomElement (f^.fWorkers)
                let f' = f&fOpenPositions .~ -1
                          &fFiring .~ Just fired
                updatePrice f'
            else updatePrice f
    | otherwise -> return f

-- | Price is updated only if hiring/firing decision is made.
updatePrice :: Firm -> RVar Firm
updatePrice f = if
    | f^.fPrice > pUpperBound * mrc -> do
        newP <- uniformAdj (-priceAdj) (f^.fPrice) 
        return $ f&fPrice .~ newP
    | f^.fPrice < pLowerBound * mrc -> do
        newP <- uniformAdj priceAdj (f^.fPrice) 
        return $ f&fPrice .~ newP
    | otherwise -> return f
  where
    mrc = f^.fWageRate / (productivity * fromIntegral daysInMonth)



