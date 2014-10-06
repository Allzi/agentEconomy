{-# LANGUAGE MultiWayIf #-}
module Month where
import Prelude hiding (sum, foldl, any, mapM_)

import Control.Lens
import Control.Monad.State.Strict hiding (mapM_)

import Simulation
import FBeginning
import HBeginning
import Day
import FEnd
import HEnd

runMonths :: Simulation ()
runMonths = do
    timer._2 .= 0
    runMonth

-- | Advances simulation by one month.
runMonth :: Simulation ()
runMonth = do
    planStep        -- from FBeginning
    sellerSearch    -- from HBeginning
    jobSearch       -- from HBeginning
    consumptionPlans-- from HBeginning
    
    runDays         -- from Day
    
    accountingStep  -- from FEnd
    incomeGetting   -- from HEnd
    adjustResWages  -- from HEnd
    timer._2 += 1
    m <- use $ timer._2

    when (m < 12) runMonth




