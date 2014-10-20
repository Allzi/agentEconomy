{-# LANGUAGE MultiWayIf #-}
module FBeginning 
( planStep
)where
import Prelude hiding (sum, foldl, any, mapM_)

import Control.Lens
import Control.Monad.State.Strict hiding (mapM_)

import Data.Random.Distribution.Bernoulli
import Data.RVar

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
    firms <%=> (adjustWage >=> fire >=> setEmpTarget >=> return . clear)
  where
    clear f = f&fMDemand .~ 0
    isFullStaff f = if (f^.fSizeTarget <= f^.fSize)
        then f&fFullStaffMonths +~ 1
        else f&fFullStaffMonths .~ 0

adjustWage :: Firm -> Simulation Firm
adjustWage f = do
    wage <- if 
        | f^.fSizeTarget > f^.fSize -> 
            sampleRVar $ uniformAdj wageAdj (f^.fWageRate)
        | f^.fFullStaffMonths < rateDropWait + 1 -> return (f^.fWageRate)
        | otherwise ->
            sampleRVar $ uniformAdj (-wageAdj) (f^.fWageRate)
    return $ f&fWageRate .~ wage

-- | Firing is done with a lag of one month from the decision.
-- Firing is not done if firm's size has gone down because of quitters.
-- Also the last worker is never fired.
fire :: Firm -> Simulation Firm
fire f = if (f^.fFiring) &&             -- Is firing decision triggered?
            (f^.fSize > 1) &&           -- Last worker is not fired.
            (f^.fSize > f^.fSizeTarget) -- Quitters are taken into account.
    then do
        hid <- sampleRVar $ randomElementN (f^.fSize) (f^.fWorkers)
        households.ix hid %= (\h -> h&hEmployer .~ Nothing)
        let newSize = (f^.fSize - 1)
        return $ f&fSize            .~ newSize
                  &fWorkers         %~ filter (/=hid)
                  &fSizeTarget      .~ newSize
    else return $ f

setEmpTarget :: Firm -> Simulation Firm
setEmpTarget f = if
    | (f^.fInventory < iLowerBound * f^.fMDemand) -> do
        let f' = f&fFiring .~ False
                  &fSizeTarget .~ max ((f^.fSize) + 1) (f^.fSizeTarget)
        risePrice f'
    | f^.fInventory > iUpperBound * f^.fMDemand -> do
        let f' = f&fFiring .~ True
                  &fSizeTarget .~ (f^.fSize) - 1
        dropPrice f'
    | otherwise -> return $
        f&fFiring .~ False
         &fSizeTarget .~ (f^.fSize)

-- | Price is risen only if hiring decision is made.
-- Rising the price happens with a certain probability, 
-- and with a random adjustment.
risePrice :: Firm -> Simulation Firm
risePrice f = do
    doesRise <- sampleRVar $ bernoulli priceAdjProb
    if doesRise && (f^.fPrice < pLowerBound * mrc f)
        then do
            newp <- sampleRVar $ uniformAdj priceAdj (f^.fPrice) 
            return $ f&fPrice .~ newp
        else return f

-- | Price drop happens only if firing decision is made.
-- Drop happens with a certain probability, and with a random adjustment.
dropPrice :: Firm -> Simulation Firm
dropPrice f = do
    doesChange <- sampleRVar $ bernoulli priceAdjProb
    if doesChange && (f^.fPrice > pUpperBound * mrc f)
        then do
            newp <- sampleRVar $ uniformAdj (-priceAdj) (f^.fPrice) 
            return $ f&fPrice .~ newp
        else return f

-- | Returns marginal cost of a firm as result.
-- Used in drop & risePrice functions.
mrc :: Firm -> Double
mrc f = f^.fWageRate / (productivity * fromIntegral daysInMonth)




