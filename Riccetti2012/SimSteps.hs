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

{--Remember:
    - avgPrice must be calculated
    - minimum wage is to-do
-}

simStep :: Simulation SimData
simStep = do
    creditStep
    mapMw updateWage
--    governmentStep
--    labourStep
--    productionStep
--    consumptionStep
--    savingStep    
--    accountingStep
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
          &pInterest .~ 0

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
    let bConstr = min (b^.bUnlendedFunds) (b^.bMaxCredit * reg3)
        loanSize = min (p^.pDebtDemand) bConstr
        interest = b^.bInterest + cbi + (riskPremium ** (p^.pLevTarg))/100
        p' = p&pDebts %~ ((b^.bID, loanSize, interest):)
              &pDebt +~ loanSize
              &pDebtDemand -~ loanSize
              &pLiquity +~ loanSize
              &pInterest .~ (p^.pInterest*p^.pDebt + interest*loanSize) / (loanSize + p^.pDebt)
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
--The Government hires public workers
--Banks buy government securities
--Central bank buys remaining securities

governmentStep :: Simulation ()
governmentStep = do
    hirePublic
    sellGovSecs

hirePublic :: Simulation ()
hirePublic = do
    g <- use government
    wids <- use workerIds
    let wn = floor ((fromIntegral workerN) * (g^.gWorkerShare))
    rids <- randSim (\rs -> randIds rs wids workerN wn)
    g' <- foldM hire g rids
    government .= g'
  where
    hire g wid = do
        Just w <- use $ workers.at wid
        let w' = w&wEmployed .~ True
        workers.ix wid.= w'
        return $ g&gWageBill +~ (w^.wWage)
        

sellGovSecs :: Simulation ()
sellGovSecs = undefined
--------------------Labour Step----------------------

labourStep :: Simulation ()
labourStep = do
    producers %= (fmap prepare)
    labourMarket
  where
    prepare p = p&pWorkers .~ 0
                 &pWageBill .~ 0

labourMarket :: Simulation ()
labourMarket = runMarket laborTrials hire workers getSeller producers getBuyer
  where
    getSeller w = if w^.wEmployed
        then Nothing
        else Just (Seller (w^.wID) (w^.wWage))
    getBuyer p = Just (Buyer (p^.pID) (p^.pLiquity))


hire :: Matcher Simulation Worker Producer
hire w p = return (w', p')
  where
    w' = w&wEmployed .~ True
    p' = p&pLiquity -~ (w^.wWage)
          &pWageBill +~ (w^.wWage)
          &pWorkers +~ 1

--------------------Production Step------------------
productionStep :: Simulation ()
productionStep = producers %= (fmap produce)
  where
    produce p = p&pProduction .~ (p^.pWorkers*productivity)

--------------------Consumption Step-------------------


consumptionStep :: Simulation ()
consumptionStep = do
    mapMp (\p -> randSim (goodSupply p))
    workers %= fmap goodDemand
    goodsMarket

goodSupply :: Producer -> [Double] -> ([Double], Producer)
goodSupply p (r:rs) = (rs, p')
  where
    price = if p^.pInventory > 0
        then p^.pPrice - genericAdj * r
        else p^.pPrice + genericAdj * r
    p' = p&pInventory +~ (p^.pProduction)
          &pPrice     .~ price

goodDemand :: Worker -> Worker
goodDemand w = w&wDemand .~ demand
                &wConsBill .~ 0
  where
    demand = if w^.wEmployed
        then w^.wWage * incomeCons + w^.wWealth * wealthCons
        else w^.wWealth * wealthCons

goodsMarket :: Simulation ()
goodsMarket = runMarket consTrials goodsMatch producers getSeller workers getBuyer
  where
    getSeller p = if p^.pInventory > 0
        then Just (Seller (p^.pID) (p^.pPrice))
        else Nothing
    getBuyer w = Just (Buyer (w^.wID) (w^.wDemand))

goodsMatch  :: Matcher Simulation Producer Worker
goodsMatch p w = return (p', w')
  where
    expenditure = undefined
    p' = undefined
    w' = w&wDemand -~ expenditure
          &wConsBill +~ expenditure

--------------------Saving Step-------------------------
savingStep :: Simulation ()
savingStep = undefined


--------------------Accounting Step----------------------
--update Producers, Banks and Households (in this order)
accountingStep :: Simulation ()
accountingStep = do
    mapMp pAccount
    mapMb bAccount
    mapMw wAccount

pAccount :: Producer -> Simulation Producer
pAccount p = undefined

bAccount :: Bank -> Simulation Bank
bAccount b = undefined

wAccount :: Worker -> Simulation Worker
wAccount w = do
    wage <- if w^.wEmployed
        then do
            collectITax (w^.wWage)
        else return 0
    let grossW = w^.wWealth + wage + w^.wDividends + w^.wInterest - w^.wConsBill
    netW <- collectWTax grossW
    return $ w&wWealth .~ netW

collectWTax :: Money -> Simulation Money
collectWTax wealth = do
    g <- use government
    price <- use avgPrice
    let tt = g^.gWTaxTreshold
        taxable = wealth - (tt * price)
    if (taxable > 0)
        then do 
            let t  = g^.gWealthTax
                tax = taxable*t
            government.gTaxIncome += tax
            return $ wealth - tax
        else return wealth

collectITax :: Money -> Simulation Money
collectITax income = do
    g <- use government
    let t = g^.gIncomeTax
        tax = income*t
    government.gTaxIncome += tax
    return $ income - tax



-------------------Solvency Step----------------------

solvencyStep :: Simulation ()
solvencyStep = undefined

