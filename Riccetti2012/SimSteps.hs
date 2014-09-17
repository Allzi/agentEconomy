{-# LANGUAGE TemplateHaskell #-}
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
    lowInventory = (fromIntegral (p^.pInventory)) < inventoryTreshold * (fromIntegral (p^.pProduction))
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
    runMarket bankTrials creditMatch banks getSeller producers getBuyer
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
              &pInterest .~ (p^.pInterest*p^.pDebt + interest*loanSize) / (loanSize + p^.pDebt)
        b' = b&bUnlendedFunds -~ loanSize
    return (b', p')

------------------Wage Step-------------------------
-- No minimum wage now!!!!
updateWage :: Worker -> Simulation Worker
updateWage w = do
    (r:rs) <- use sRandoms
    ap <- use avgPrice
    sRandoms .= rs
    let wage = max ap $ if w^.wEmployed
            then w^.wWage * (1+r*genericAdj)
            else w^.wWage * (1-r*genericAdj)
    return $ w&wWage .~ wage
              &wEmployed .~ False

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
sellGovSecs = do
    -- Decifit and supply of securities:
    g <- use government
    let deficit = g^.gIntToBanks + g^.gWageBill - g^.gTaxIncome + g^.gRescueCosts
        newDebt = g^.gPubDebt + deficit
    government . gPubDebt .= newDebt 
    -- Sell to banks:
    itb <- if newDebt > 0
        then do
            bs <- use banks
            let totDemand = Map.foldl pdDemand 0 bs
                demandSat = if totDemand == 0 
                    then 1
                    else min 1 (newDebt / totDemand)
                banksShare = max 1 (totDemand/newDebt)
            banks %= buyPubDebt demandSat
            return (banksShare * newDebt * g^.gInterest) --CB buys rest
        else
            return 0
    let g'= g&gIntToBanks .~ itb
             &gPubDebt .~ newDebt
    government .= g'
  where
    pdDemand acc b = undefined
    buyPubDebt b = undefined
    

--------------------Labour Step----------------------

labourStep :: Simulation ()
labourStep = do
    producers %= (fmap prepare)
    labourMarket
  where
    prepare p = p&pWorkers .~ 0
                 &pWageBill .~ 0
                 &pLiquity .~ (p^.pNetWorth + p^.pDebt)

labourMarket :: Simulation ()
labourMarket = runMarket workerTrials hire workers getSeller producers getBuyer
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


--------------------Consumption Step-------------------
-- Production and consumtion takes place here
-- Issues:
    -- Only wageBill is taken into account when calculating average cost

consumptionStep :: Simulation ()
consumptionStep = do
    mapMp (\p -> randSim (goodSupply p))
    mapMw goodDemand
    goodsMarket

goodSupply :: Producer -> [Double] -> ([Double], Producer)
goodSupply p (r:rs) = (rs, p')
  where
    production = p^.pWorkers * productivity
    price = if (p^.pInventory == 0) && (production > 0)
        then p^.pPrice + genericAdj * r
        else p^.pPrice - genericAdj * r
    avgCost = if production > 0
        then p^.pWageBill/(fromIntegral production)
        else 0
    p' = p&pInventory +~ production
          &pPrice     .~ (max avgCost price)
          &pProduction .~ production
          &pSales .~ 0

goodDemand :: Worker -> Simulation Worker
goodDemand w = do
    ap <- use avgPrice
    let actual = if demand < ap
        then min ap $ income + w^.wWealth
        else demand
    return $ w&wDemand .~ actual
              &wConsBill .~ 0
  where
    demand = income * incomeCons + w^.wWealth * wealthCons
    income = if w^.wEmployed
        then w^.wWage
        else 0

goodsMarket :: Simulation ()
goodsMarket = runMarket producerTrials goodsMatch producers getSeller workers getBuyer
  where
    getSeller p = if p^.pInventory > 0
        then Just (Seller (p^.pID) (p^.pPrice))
        else Nothing
    getBuyer w = Just (Buyer (w^.wID) (w^.wDemand))

goodsMatch  :: Matcher Simulation Producer Worker
goodsMatch p w = return (p', w')
  where
    price = p^.pPrice
    amount = min (floor ((w^.wDemand)/price)) (p^.pInventory)
    expenditure = price * (fromIntegral amount)
    p' = p&pInventory -~ amount
          &pSales +~ expenditure
    w' = w&wDemand -~ expenditure
          &wConsBill +~ expenditure

--------------------Saving Step-------------------------
savingStep :: Simulation ()
savingStep = do
    mapMw (\w -> randSim (savingsSupply w))
    mapMb (\b -> randSim (savingsDemand b))
    depositMarket

savingsSupply :: Worker -> [Double] -> ([Double], Worker)
savingsSupply w (r:rs) = (rs, w')
  where
    w' = undefined
    --minimum interest

savingsDemand :: Bank -> [Double] -> ([Double], Bank)
savingsDemand b (r:rs) = (rs, b')
  where
    b' = undefined
    -- update bDepRate

depositMarket :: Simulation ()
depositMarket = runMarket workerTrials depositMatch workers getSeller banks getBuyer
  where
    getSeller w = undefined
    getBuyer b = Just (Buyer (b^.bID) (b^.bDepRate))

depositMatch :: Matcher Simulation Worker Bank
depositMatch w b = return (w', b')
  where
    w' = undefined
    b' = undefined

--------------------Accounting Step----------------------
--update Producers, Banks and Households (in this order)
accountingStep :: Simulation ()
accountingStep = do
    aggDividends .= 0
    mapMp pAccount
    mapMb bAccount
    -- Dividents!
    mapMw wAccount

pAccount :: Producer -> Simulation Producer
pAccount p = do
    -- gross profit
    let oldProf = if p^.pProfit > 0
            then 0
            else p^.pProfit
        prof = oldProf + p^.pSales - p^.pWageBill - p^.pInterest * p^.pDebt
    -- Determine payout rate, calculate net profit
    r <- get1Rand
    let payOut = min 1 $ max 0 $ if (p^.pInventory == 0) && (p^.pProduction > 0)
            then p^.pPayOutR * (1+r)
            else p^.pPayOutR * (1-r)
    netProf <- (payDividends payOut) =<< collectITax prof    
    -- Net worth and solvency
    let worth = p^.pNetWorth - netProf
    nWorth <- collectWTax worth
    let isSolvent = nWorth > 0
    unless isSolvent (badDebts nWorth (p^.pDebts))
    let p' = p&pPayOutR .~ payOut
              &pProfit .~ prof
              &pNetProfit .~ netProf
              &pNetWorth .~ nWorth
              &pIsSolvent .~ isSolvent
    return p'

badDebts :: Money -> [(Bid, Money, Double)] -> Simulation ()
badDebts worth debts = do
    when (null debts) $ error "no creditors"
    let total = sum (fmap calcDebt debts)
        ratio = 1 - (total+worth) / total 
    mapM_ (creditLoss ratio) debts
  where
    calcDebt (_, d, i) = d*(1+i)
    creditLoss ratio (bid, d, i) = do
        let loss = ratio*d*(1+i)
        banks . ix bid . bBadDebt += loss
    

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
collectITax income = if income > 0
    then do
        g <- use government
        let t = g^.gIncomeTax
            tax = income*t
        government.gTaxIncome += tax
        return $ income - tax
    else return income

payDividends :: Double -> Money -> Simulation Money
payDividends rate prof = if prof > 0
    then do
        aggDividends += prof*rate
        return (prof*(1-rate))
    else return prof

-------------------Solvency Step----------------------

solvencyStep :: Simulation ()
solvencyStep = undefined

