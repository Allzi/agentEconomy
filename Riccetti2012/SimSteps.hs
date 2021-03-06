{-# LANGUAGE TemplateHaskell #-}
module SimSteps where
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.IntMap as Map
import Data.List
import Data.Function
import Debug.Trace
import System.Random
import Data.Maybe

import AgentTypes
import Simulation
import SimUtils

{-
- Remember:
    - minimum wage is to-do
    - behavior income streams must be checked (something is forgotten for sure)
- Issues:
    - interests on deposits are not paid on correct rounds atm
    - CB money setting behavior is a BIG question
    - market mechanism's imperfect information needs a fix?
-}

simStep :: Simulation SimData
simStep = do
    t <- use timer
    creditStep
    mapMw updateWage
    governmentStep
    labourStep
    consumptionStep
    savingStep    
    accountingStep
    solvencyStep
    
    timer += 1
    -- Comes from Simulation.hs  
    collectData


-------------------Credit Step---------------------
--Firms plan for credit demand
--Banks plan for supply
--Credit market

creditStep :: Simulation ()
creditStep = do
    cBActions
    mapMp (\p -> randSim (debtDemand p))
    mapMb (\b -> randSim (debtSupply b))
    runMarket creditMarket
    
cBActions :: Simulation ()
cBActions = do
    bs <- use banks
    ps <- use producers
    let exd = Map.foldl (\acc p -> acc + p^.pDebtDemand) 0 ps
        exs = Map.foldl (\acc b -> acc + b^.bUnlendedFunds) 0 bs
    r <- get1Rand
    if exd > exs
        then cbMoney*= (1+genericAdj*r)
        else cbMoney*= (1-genericAdj*r)
    cbc <- use cbMoney
    cbi <- use cbInterest
    let cred = cbc/(fromIntegral bankN)
        cost = cred * cbi
    banks %= fmap (\b -> b&bCBCredit   .~ cred
                          &bCBCredCost .~ cost)

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
          &pDebts .~ []
          &pInterest .~ 0

debtSupply :: Bank -> [Double] -> ([Double], Bank)
debtSupply b (r:rs) = (rs, b')
  where
    constraint1 = reg1 * b^.bNetWorth
    constraint2 = reg2 * b^.bNetWorth + b^.bDeposits + b^.bCBCredit
    supply = min constraint1 constraint2
    interest = if (b^.bUnlendedFunds) == 0
        then (b^.bInterest)*(1+genericAdj*r)
        else (b^.bInterest)*(1-genericAdj*r)
    b' = b&bMaxCredit       .~ supply
          &bUnlendedFunds   .~ supply
          &bInterest        .~ interest
          &bPDDemand        .~ constraint2
    
creditMarket :: Market SimState Bank Producer
creditMarket = Market {
    _mTrials    = bankTrials,
    _mMatcher   = creditMatch,
    _mSuppliers = banks,
    _mGetSeller = getSeller,
    _mDemanders = producers,
    _mGetBuyer  = getBuyer
    }
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
              &bLendIncome +~ loanSize * interest
              &bPDDemand -~ loanSize
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
              &wWageIncome .~ 0

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
    g' <- foldM hire (g&gWageBill.~0) rids
    government .= g'
  where
    hire g wid = do
        Just w <- use $ workers.at wid
        let w' = w&wEmployed .~ True
                  &wWageIncome .~ (w^.wWage)
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
            mapMb (buyPubDebt demandSat (g^.gInterest))
            return (banksShare * newDebt * g^.gInterest) --CB buys rest
        else
            return 0
    let g'= g&gIntToBanks .~ itb
             &gPubDebt .~ newDebt
             &gWageBill .~ 0
             &gRescueCosts .~ 0
    government .= g'
  where
    pdDemand acc b = acc + b^.bPDDemand
    buyPubDebt amount i b = return $ b&bPDIncome .~ (b^.bPDDemand * amount * i)
                                      &bPD .~ (b^.bPDDemand * amount)

--------------------Labour Step----------------------

labourStep :: Simulation ()
labourStep = do
    producers %= (fmap prepare)
    runMarket labourMarket
  where
    prepare p = p&pWorkers .~ 0
                 &pWageBill .~ 0
                 &pLiquity .~ (p^.pNetWorth + p^.pDebt)

labourMarket :: Market SimState Worker Producer
labourMarket = Market {
    _mTrials    = workerTrials,
    _mMatcher   = hire,
    _mSuppliers = workers,
    _mGetSeller = getSeller,
    _mDemanders = producers,
    _mGetBuyer  = getBuyer
    }
  where
    getSeller w = if w^.wEmployed
        then Nothing
        else Just (Seller (w^.wID) (w^.wWage))
    getBuyer p = Just (Buyer (p^.pID) (p^.pLiquity))


hire :: Matcher Simulation Worker Producer
hire w p = return (w', p')
  where
    w' = w&wEmployed .~ True
          &wWageIncome .~ (w^.wWage)
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
    runMarket goodsMarket

goodSupply :: Producer -> [Double] -> ([Double], Producer)
goodSupply p (r:rs) = (rs, p')
  where
    production = p^.pWorkers * productivity
    price = if (p^.pInventory == 0) && (production > 0)
        then p^.pPrice * (1 + genericAdj * r)
        else p^.pPrice * (1 - genericAdj * r)
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
        then min ap $ w^.wWageIncome + w^.wWealth
        else demand
    return $ w&wDemand .~ actual
              &wConsBill .~ 0
  where
    demand = w^.wWageIncome * incomeCons + w^.wWealth * wealthCons

goodsMarket :: Market SimState Producer Worker
goodsMarket = Market {
    _mTrials    = producerTrials,
    _mMatcher   = goodsMatch,
    _mSuppliers = producers,
    _mGetSeller = getSeller,
    _mDemanders = workers,
    _mGetBuyer  = getBuyer
    }
  where
    getSeller p = if p^.pInventory > 0
        then Just (Seller (p^.pID) (p^.pPrice))
        else Nothing
    getBuyer w = if w^.wDemand > 0
        then Just (Buyer (w^.wID) (w^.wDemand))
        else Nothing

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
    cbint <- use cbInterest
    mapMw (\w -> randSim (savingsSupply w))
    mapMb (\b -> randSim (savingsDemand cbint b))
    runMarket depositMarket

savingsSupply :: Worker -> [Double] -> ([Double], Worker)
savingsSupply w (r:rs) = (rs, w')
  where
    minInterest = if w^.wInterest > 0 --if got a deal last time
        then w^.wMinInterest * (1 + genericAdj * r) 
        else w^.wMinInterest * (1 - genericAdj * r)
    w' = w&wMinInterest .~ minInterest
          &wInterest .~ 0
          &wDepSupply .~ (w^.wWealth + w^.wWageIncome - w^.wConsBill)

savingsDemand :: Double -> Bank -> [Double] -> ([Double], Bank)
savingsDemand cbi b (r:rs) = (rs, b')
  where
    depRate = if b^.bPDDemand > b^.bPD
        then min cbi $ b^.bDepRate * (1 - genericAdj * r)
        else b^.bDepRate * (1 + genericAdj *r)
    b' = b&bDepRate .~ depRate
          &bDeposits .~ 0
          &bDepCost .~ 0

depositMarket :: Market SimState Worker Bank
depositMarket = Market {
    _mTrials    = workerTrials,
    _mMatcher   = depositMatch,
    _mSuppliers = workers,
    _mGetSeller = getSeller,
    _mDemanders = banks,
    _mGetBuyer  = getBuyer
    }
  where
    getSeller w = if w^.wDepSupply > 0
        then Just (Seller (w^.wID) (w^.wMinInterest))
        else Nothing
    getBuyer b = Just (Buyer (b^.bID) (b^.bDepRate))

depositMatch :: Matcher Simulation Worker Bank
depositMatch w b = return (w', b')
  where
    amount = w^.wDepSupply
    cost = b^.bDepRate * amount
    w' = w&wDepSupply .~ 0
          &wInterest +~ cost
    b' = b&bDeposits +~ amount
          &bDepCost +~ cost

--------------------Accounting Step----------------------
--update Producers, Banks and Households (in this order)
--no dividends!!
accountingStep :: Simulation ()
accountingStep = do
    aggDividends .= 0
    mapMp pAccount
    mapMb bAccount
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
    let worth = p^.pNetWorth + netProf
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
bAccount b = do
    -- gross profit
    let oldProf = if b^.bProfit > 0
            then 0
            else b^.bProfit
        prof = oldProf + b^.bLendIncome + b^.bPDIncome 
            - b^.bBadDebt - b^.bDepCost - b^.bCBCredCost
    -- Determine payout rate, calculate net profit
    r <- get1Rand
    let payOut = min 1 $ max 0 $ if b^.bUnlendedFunds > 0 || b^.bMaxCredit == 0
            then b^.bPayOutR * (1+r)
            else b^.bPayOutR * (1-r)
    netProf <- (payDividends payOut) =<< collectITax prof
    -- Net worth and solvency
    let worth = b^.bNetWorth + netProf
    nWorth <- collectWTax worth
    let isSolvent = nWorth > 0
        b' = b&bPayOutR .~ payOut
              &bProfit .~ prof
              &bNetProfit .~ netProf
              &bNetWorth .~ nWorth
              &bIsSolvent .~ isSolvent
              &bBadDebt .~ 0
    return b'


wAccount :: Worker -> Simulation Worker
wAccount w = do
    netWage <- collectITax (w^.wWageIncome)
      
    let grossW = w^.wWealth + netWage + w^.wInterest - w^.wConsBill
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
solvencyStep = do
    pSolvency
    bSolvency
    distrDiv

pSolvency :: Simulation ()
pSolvency = do
    ps <- use producers
    let (solvents, exits) = Map.partition (\p -> p^.pIsSolvent) ps
    avgPrice .= avgP solvents
    entrants <- mapMOf traverse entrantFirm exits
    producers .= (Map.union solvents entrants)
  where
    avgP ps = (Map.foldl (\acc p -> p^.pPrice+acc) 0 ps) / (fromIntegral (Map.size ps))

entrantFirm :: Producer -> Simulation Producer
entrantFirm p = do
    nr <- sampleNormal 3 1
    ap <- use avgPrice
    let nw = (max 0.1 nr) * ap
        e = makeProducer (p^.pID)
    aggDividends -= nw
    return $ e&pPrice .~ ap
              &pNetWorth .~ nw

bSolvency :: Simulation ()
bSolvency = do
    bs <- use banks
    let (solvents, exits) = Map.partition (\b -> b^.bIsSolvent) bs
    when (Map.null solvents) (error "no solvent banks!")
    avgInterest .= avgI solvents
    entrants <- mapMOf traverse entrantBank exits
    banks .= (Map.union solvents entrants)
  where
    avgI bs = (Map.foldl (\acc b -> b^.bInterest+acc) 0 bs) / (fromIntegral (Map.size bs))


entrantBank :: Bank -> Simulation Bank
entrantBank b = do
    nr <- sampleNormal 5 1
    ap <- use avgPrice
    ai <- use avgInterest
    let nw = (max 0.2 nr) * ap
        e = makeBank (b^.bID)
    aggDividends -= nw
    return $ e&bInterest .~ ai
              &bNetWorth .~ nw

distrDiv :: Simulation ()
distrDiv = do
    ws <- use workers
    ds <- use aggDividends
    let aggNw = Map.foldl (\acc w -> acc + w^.wWealth) 0 ws
    if aggNw > (-ds)
        then do
            workers .= (fmap (\w -> w&wWealth *~ (aggNw+ds)/aggNw )ws)

        else do
            workers .= (fmap (\w -> w&wWealth .~ 0.01) ws)
            government . gRescueCosts += ((-ds) - aggNw)

