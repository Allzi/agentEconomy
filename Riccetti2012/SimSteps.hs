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
    ps <- use producers
    bs <- use banks
    let buyers = fmap getDemand $ Map.elems ps
        sellers = fmap getSupply $ Map.elems bs
    marketLoop creditTrials creditMatch sellers buyers
  where
    getDemand :: Producer -> Buyer
    getDemand p = Buyer (p^.pID) (10000) -- no real limit to interest
    getSupply :: Bank -> Seller
    getSupply b = Seller (b^.bID) (b^.bInterest)
    
creditMatch :: Seller -> Buyer -> Simulation (Seller, Buyer)
creditMatch seller buyer = do
    let pid = buyerId buyer
        bid = sellerId seller
    -- get p and b
    Just p <- use $ producers.at pid
    Just b <- use $ banks.at bid
    cbi <- use cbInterest
    -- determine loan size and interest
    let bConstr = min (b^.bUnlendedFunds) (b^.bMaxCredit * reg3)
        loanSize = min (p^.pDebtDemand) bConstr
        interest = (askPrice seller) + cbi + (riskPremium ** (p^.pLevTarg))/100
    -- producer:
        -- gets the needed funds, or all the possible funds to get
        -- remembers the bank
    -- bank reduces unlended funds
    let p' = p&pDebts %~ ((bid, loanSize, interest):)
              &pDebt +~ loanSize
              &pDebtDemand -~ loanSize
        b' = b&bUnlendedFunds -~ loanSize
        seller' = if (b'^.bUnlendedFunds) == 0
            then Seller bid 0
            else Seller bid (b^.bInterest)
        buyer' = if (p'^.pDebtDemand) == 0
            then Buyer pid 0
            else Buyer pid 10000
    producers.ix pid .= p'
    banks.ix bid .= b'
    return (seller', buyer')
    
------------------Government Step-------------------
--The Government hires public workers
--Banks buy government securities
--Central bank buys remaining securities

governmentStep :: Simulation ()
governmentStep = undefined

--------------------Labour Step----------------------

labourStep :: Simulation ()
labourStep = undefined

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



