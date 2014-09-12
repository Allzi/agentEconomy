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



--REMEMBER:
--calculate pInterest from debts before repayment

simStep :: Simulation SimData
simStep = do
    creditStep
    mapMw updateWage
--    governmentStep
--    labourStep
--    productionStep
--    consumptionStep
--    moneyStep
--    solvencyStep

    timer += 1
    -- Comes from Simulation.hs  
    collectData


-------------------Credit Step---------------------
--Firms plan for credit demand
--Banks plan for supply
--Credit market

creditStep :: Simulation ()
creditStep = do
    mapMp (\p -> randSim (debtDemand p))
    mapMb (\b -> randSim (debtSupply b))
    creditMarket

debtDemand :: Producer -> [Double] -> ([Double], Producer)
debtDemand p (r:rs) = (rs, p')
  where
    netWorth = p^.pNetWorth
    interestRate = p^.pInterest
    isGoodInv = p^.pProfit > (p^.pNetWorth + p^.pDebt) * interestRate
    lowInventory = p^.pInventory < inventoryTreshold * p^.pProduction
    targetMult = if (isGoodInv && lowInventory)
        then (1+r*genericAdj)
        else (1-r*genericAdj)
    p' = p&pLevTarg *~ targetMult
          &pDebtDemand .~ (targetMult * netWorth)
          &pDebt .~ 0

debtSupply :: Bank -> [Double] -> ([Double], Bank)
debtSupply b (r:rs) = (rs, b')
  where
    constraint1 = reg1*b^.bNetWorth
    constraint2 = reg2*b^.bNetWorth + b^.bDeposits + b^.bCBCredit
    supply = min constraint1 constraint2
    interest = if (b^.bUnlendedFunds) == 0
        then (b^.bInterest)*(1+genericAdj*r)
        else (b^.bInterest)*(1-genericAdj*r)
    b' = b&bMaxCredit       .~ supply
          &bUnlendedFunds   .~ supply
          &bInterest        .~ interest
    
creditMarket :: Simulation ()
creditMarket = do
    runMarket creditTrials creditMatch banks getSeller producers getBuyer
  where
    getBuyer :: Producer -> Maybe Buyer
    getBuyer p = if (p^.pDebtDemand) == 0
        then Nothing
        else Just (Buyer (p^.pID) (10000)) -- no real limit to interest
    getSeller :: Bank -> Maybe Seller
    getSeller b = if (b^.bUnlendedFunds) == 0
        then Nothing
        else Just (Seller (b^.bID) (b^.bInterest))
    
creditMatch :: Bank -> Producer -> Simulation (Bank, Producer)
creditMatch b p = do
    cbi <- use cbInterest
    -- determine loan size and interest
    let bConstr = min (b^.bUnlendedFunds) (b^.bMaxCredit * reg3)
        loanSize = min (p^.pDebtDemand) bConstr
        interest = b^.bInterest + cbi + (riskPremium ** (p^.pLevTarg))/100
    -- producer:
        -- gets the needed funds, or all the possible funds to get
        -- remembers the bank
    -- bank reduces unlended funds
    let p' = p&pDebts %~ ((b^.bID, loanSize, interest):)
              &pDebt +~ loanSize
              &pDebtDemand -~ loanSize
        b' = b&bUnlendedFunds -~ loanSize
    return (b', p')

------------------Wage Step-------------------------
-- No minimum wage now!!!!
updateWage :: Worker -> Simulation Worker
updateWage w = do
    (r:rs) <- use sRandoms
    sRandoms .= rs
    if w^.wEmployed
        then return (w&wWage      *~ (1+r*genericAdj)
                      &wEmployed .~ False)
        else return (w&wWage *~ (1-r*genericAdj))

------------------Government Step-------------------
--Tax collection?
--The Government hires public workers
--Banks buy government securities
--Central bank buys remaining securities

governmentStep :: Simulation ()
governmentStep = undefined

--------------------Labour Step----------------------

labourStep :: Simulation ()
labourStep = undefined


labourMarket :: Simulation ()
labourMarket = do
    runMarket laborTrials hire workers getSeller producers getBuyer
  where
    getSeller w = if w^.wEmployed
        then Nothing
        else Just (Seller (w^.wID) (w^.wWage))
    getBuyer p = undefined


hire :: Matcher Simulation Worker Producer
hire seller buyer = do
    let seller' = undefined
        buyer'  = undefined
    return (seller', buyer')

--------------------Production Step------------------

productionStep :: Simulation ()
productionStep = undefined

--------------------Consumption Step-------------------

consumptionStep :: Simulation ()
consumptionStep = undefined

--------------------Money Step----------------------

moneyStep :: Simulation ()
moneyStep = undefined

-------------------Solvency Step----------------------

solvencyStep :: Simulation ()
solvencyStep = undefined



