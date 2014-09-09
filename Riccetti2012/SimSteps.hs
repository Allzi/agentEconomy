module SimSteps where
import Control.Lens
import Control.Monad.State
import qualified Data.IntMap as Map
import Data.List
import Data.Function
import Debug.Trace
import System.Random
import Data.Maybe

import AgentTypes
import Simulation
import SimUtils

simStep :: Simulation SimData
simStep = do
--    creditStep    
--    governmentStep
--    labourStep
--    productionStep
--    consumptionStep
--    moneyStep
--    solvencyStep

    timer += 1
    -- Comes from Simulation.hs  
    collectData


---------------Credit Step---------------------
--Firms plan for credit demand
--Banks plan for supply
--Credit market

creditStep :: Simulation ()
creditStep = do
    mapMp (\p -> randSim (debtDemand p))
    --supply
    creditMarket


debtDemand :: Producer -> [Double] -> ([Double], Producer)
debtDemand p (r:rs) = (rs, p')
  where
    netWorth = p^.pNetWorth
    interestRate = undefined
    isGoodInv = p^.pProfit > (p^.pNetWorth + p^.pDebt) * interestRate
    lowInventory = p^.pInventory < inventoryTreshold * p^.pProduction
    targetMult = if (isGoodInv && lowInventory)
        then (1+r*levAdj) 
        else (1-r*levAdj)
    p' = p&pLevTarg *~ targetMult

debtSupply :: Bank -> Bank
debtSupply b = undefined 
    
creditMarket :: Simulation ()
creditMarket = undefined


----------------Government Step----------------
--The Government hires public workers
--Banks buy government securities
--Central bank buys remaining securities

governmentStep :: Simulation ()
governmentStep = undefined
