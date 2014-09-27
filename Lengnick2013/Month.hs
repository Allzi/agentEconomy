{-# LANGUAGE MultiWayIf #-}
module Month where
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.IntMap as Map
import Data.List
import Control.Applicative
import Debug.Trace
import System.Random
import Data.Random

import AgentTypes
import Simulation
import SimUtils
import Day


-- | Advances simulation by one month.
-- TODO: add actual firing of employee
runMonth :: Simulation ()
runMonth = do
    planStep
    searchStep    
    
    -- day x21    
    
    -- End of month:
    -- accounting (wages, dividents)
    -- adjustReservation


-- | In planStep all the firms prepare for next month.
-- Offered wage rates are adjusted up if there are unfilled positions,
-- and down if there are enought months of full staff.
-- Hiring and firing happens according to inventory and past demand.
-- If hiring or firing happens, prices are adjusted using a sort of mark-up pricing.
-- In the end the records are cleared for next month.
planStep :: Simulation ()
planStep = firms <$=> (adjustWage >=> hire >=> return . clear)
  where
    adjustWage :: Firm -> Simulation Firm
    adjustWage f = do
        -- Adjust wage rate
        wage <- lift $ if 
            | f^.fOpenPositions /= 0 -> 
                uniformAdj wageAdj (f^.fWageRate)
            | f^.fFullStaffMonths < rateDropWait -> return (f^.fWageRate)
            | otherwise -> uniformAdj (-wageAdj) (f^.fWageRate)
        return $ f&fWageRate .~ wage
    hire :: Firm -> Simulation Firm
    hire f = lift $ if
        | f^.fInventory > iUpperBound * f^.fMDemand -> do
            let f' = f&fOpenPositions +~ 1
            updatePrice f'
        | f^.fInventory < iLowerBound * f^.fMDemand -> do
            fired <- randomElement (f^.fWorkers)
            let f' = f&fOpenPositions .~ 0
                      &fFiring .~ Just fired
            updatePrice f'
        | otherwise -> return f
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


-- | In searchStep households act in goods and labor markets.
-- Labor market search happens in shuffled order.
searchStep :: Simulation ()
searchStep = do
    return ()

-- | An utility function, that adjusts uniformly the double given as the second
-- argument. The first argument tells the maximum relative adjustment.
-- Giving negative maximum adjustment makes the adjustment negative.
uniformAdj :: Double -> Double -> RVar Double
uniformAdj adj a = (\r -> a * (1 + adj * r)) <$> stdUniform

