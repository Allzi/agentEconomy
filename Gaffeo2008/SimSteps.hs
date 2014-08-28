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

simStep :: Simulation SimData
simStep = do
    solvensyStep
    devStep
    planStep
    laborStep
    creditStep
    productionStep
    consStep
    moneyStep
    timer += 1
    -- Comes from Simulation.hs    
    collectData

--------------Generic stuff-------------------
simplePStep :: ([Double] -> Producer -> ([Double], Producer)) -> Simulation()
simplePStep f = do
    sim <- get
    let rs = sim^.sRandoms
        prs = sim^.producers
        (rs', newProducers) =  Map.mapAccum f rs prs
    producers .= newProducers
    sRandoms .= rs'

--very slow random index taker
randIds :: Eq a => [Double] -> [a] -> Int -> Int-> ([Double],[a])
randIds rs _ _ 0 = (rs, [])
randIds rs [] _ _ = (rs, [])
randIds (r:rs) ids idNum toTake = (rs', i:ls)
  where 
    (rs', ls) = randIds rs ids' (idNum-1) (toTake-1)
    rind = floor $ r*fromIntegral idNum
    i = ids !! rind
    ids' = delete i ids

shuffle ::Eq a => [a]-> Int -> Simulation [a]
shuffle l n = do
    rs <- use sRandoms
    let (rs', shuffled) = randIds rs l n n
    sRandoms .= rs'
    return shuffled

---------------Solvensy checks------------------
solvensyStep :: Simulation ()
solvensyStep = do 
    rs <- use sRandoms
    fs <- use producers
    bs <- use banks
    let (rs', nFirms) = insolvensies rs fs
    let (rs'', nBanks) = insolvensies rs' bs
    producers .= nFirms
    banks .= nBanks
    sRandoms .= rs'' 

-- atm gives error if no solvent firms
insolvensies :: Firm a => [Double] -> Map.IntMap a -> ([Double], Map.IntMap a)
insolvensies rs fs = (rs', Map.union newFs goodFs)
  where
    (goodFs, badFs) = Map.partition isSolvent fs
    goodList = Map.elems goodFs
    (rs', newFs) = Map.mapAccum (randEntrant goodList (length goodList)) rs badFs

--replaces something with a random element of a list
randEntrant :: Firm a => [a] -> Int -> [Double] -> a -> ([Double], a)
randEntrant selection num (r:rs) _ = (rs, a')
  where
    a = selection !! floor (fromIntegral num *r)
    a' = makeEntrant a

------------------New productivities---------------------
devStep :: Simulation ()
devStep = simplePStep develop

develop :: [Double] -> Producer -> ([Double] ,Producer)
develop (r:rs) pr = (rs, nprod)
  where
    nprod = pr&pProductivity +~ exponNum r mu
              &pCash -~ contr
    mu = if contr > 0
            then contr * rdinv1
            else 0 
    contr = pr^.pProfit / pr^.pNomSales

-- takes a random number in [0,1] and mean
exponNum :: Double -> Double -> Double
exponNum r mu = (-mu) * log r

----------------Plans for production-------------------
planStep :: Simulation ()
planStep = do
    pl <- use priceLevel
    simplePStep (updatePlan pl)

updatePlan :: Money -> [Double] -> Producer -> ([Double] ,Producer)
updatePlan p (r:rs) prod = if prod'^.pPrice < prod'^.pAC
    then (rs, prod'&pPrice .~ prod'^.pAC)
    else (rs, prod')
  where 
      prod'
        | isNothing (prod^.pGoods)  && (prod^.pPrice < p)
            = prod&pPrice *~ (1+r*priceGrowth)
        | isJust (prod^.pGoods) && (prod^.pPrice > p) 
            = prod&pPrice *~ (1-r*priceGrowth)
        | isNothing (prod^.pGoods)
            = prod&pQuantity *~ (1+r*quantityGrowth)
        | otherwise
            = prod&pQuantity *~ (1-r*quantityGrowth)

------------------Labor markets------------------------
-- Issues:
--     - Old wages don't update (should update after some time)
--     - No minimum wage
--     - not good at all
laborStep :: Simulation ()
laborStep = do
    sim <- get
    --empty proposals
    producers %= fmap (\a -> a&pNAppls.~[])
    --shuffle the order of workers
    shuffled <- shuffle (sim^.workerIds) workerN
    --each worker sends applications
    mapM_ searchJob (sim^.workerIds)
    --remove old employment statuses for hiring
    workers %= fmap (\a -> a&wEmployer .~ Nothing
                            &wIncome .~ 0)
    --hiring
    pids <- use producerIds
    shuffleds <- shuffle pids producerN
    mapM_ hire shuffleds

searchJob :: Wid -> Simulation ()
searchJob wid = do
    Just w <- use $ workers.at wid -- can fail, if something stupid is happening
    pids <- use producerIds
    case w^.wEmployer of
        Just eid -> do
            Just emp <- use $ producers . at eid
            if not $ null (emp^.pWorkers)
                then otherApps (delete wid pids) (producerN-1) (laborTrials-1)
                else otherApps pids producerN laborTrials
        Nothing -> otherApps pids producerN laborTrials
  where
    otherApps :: [Pid] -> Int -> Int -> Simulation ()
    otherApps pids pN trialN = do
        rs <- use sRandoms
        let (rs', pids') = randIds rs pids pN trialN
        mapM_ (\p -> producers . ix p . pNAppls %= (wid:)) pids'
        sRandoms .= rs'

hire :: Pid -> Simulation ()
hire pid = do
    calcOffer
    -- Just a little bit sorting :)
    producers . ix pid . pWorkers %= sortBy (compare `on` snd)
    Just p <- use $ producers . at pid -- Can fail
    let ow = p^.pWorkers
        nas = p^.pNAppls
        w = p^.pEntrantWage
        nw = fmap (\a -> (a,w)) nas
        allw = ow ++ nw
    producers . ix pid . pWorkers .= []
    mapM_ tryHire allw 
  where
    tryHire :: (Wid, Money) -> Simulation ()
    tryHire a@(wid, wage) = do
        Just w <- use $ workers.at wid
        Just p <- use $ producers.at pid
        when ((length (p^.pWorkers) < p^.pLDemand) && isNothing (w^.wEmployer)) $ do
            workers . ix wid . wEmployer .= Just pid
            workers . ix wid . wIncome .= wage
            producers . ix pid . pWorkers %= (a:)
            producers . ix pid . pCash -= wage
    calcOffer :: Simulation ()
    calcOffer = do
        Just p <- use $ producers . at pid -- Can fail
        let ldemand = ceiling (p^.pQuantity / p^.pProductivity)
        producers . ix pid . pLDemand .= ldemand
        when (ldemand > length  (p^.pWorkers)) $ do
            (r:rs) <- use sRandoms
            producers . ix pid . pEntrantWage *= (1+r*wageGrowth)
            sRandoms .= rs

---------------Credit markets---------------
creditStep :: Simulation ()
creditStep = do
    pids <- use producerIds
    shuffled <- shuffle pids producerN
    mapM_ getFinance pids
    bs <- use banks
    rate <- use iRate
    banks .= fmap (bankFinance rate) bs
  
getFinance :: Pid -> Simulation ()
getFinance pid = do
    Just p <- use $ producers . at pid
    bIds <- use bankIds
    let need = -(p^.pCash)
    when (need>0) $ do
        -- get random banks
        rs <- use sRandoms
        let (rs', bs) = randIds rs bIds bankN creditTrials
        sRandoms .= rs'
        --get offers and choose best
        let leverage = need/(p^.pClosing)
        offers <- mapM (getOffer leverage) bs
        let (bid, offer)= minimumBy (compare `on` snd) offers
        --update debts and cash
        let debt = need*offer
        let p' = p & pDebt .~ Just (bid, debt)
                   & pCash +~ need
        producers . ix pid .= p'
        banks . ix pid . bCash -= need
  where
    getOffer :: Double -> Bid -> Simulation (Bid, Double)
    getOffer leverage bId = do
        (r:rs) <- use sRandoms
        cRate <- use iRate
        Just b <- use $ banks . at bId
        let mUp = b^.bMarkUp
        let rate = cRate * (1 + r * bankCosts * mUp leverage)
        return (bId, rate)

bankFinance :: Double -> Bank -> Bank
bankFinance rate b =
    if need>0
      then b&bCash .~ 0
            &bDebt .~ need * (1+rate)
      else b
  where
    need = -(b^.bCash)

-----------------Production-------------------
productionStep :: Simulation ()
productionStep = do
    ps <- use producers
    producers .= fmap produce ps

produce :: Producer -> Producer
produce p = p&pGoods .~ Just goods
             &pAC .~ ac
             &pNomSales .~ 0
  where
    goods = wnum * (p^.pProductivity) 
    wnum = fromIntegral $ length (p^.pWorkers)
    wc = foldl (\acc (_,w) -> acc + w) 0 (p^.pWorkers)
    ac = wc/goods
    
-----------------Consumption-------------------
consStep :: Simulation ()
consStep = do
    aRSales .= 0
    aNSales .= 0
    wids <- use workerIds
    shuffle wids workerN
    mapM_ consume wids
    nSales <- use aNSales
    rSales <- use aRSales
    priceLevel .= nSales / rSales

consume :: Wid -> Simulation ()
consume wid = do
    Just w <- use $ workers . at wid
    rs <- use sRandoms
    pids <- use producerIds
    let income = w^.wIncome
        demand = if income>0 
            then income * mrC1
            else (w^.wSavings) * mrC2
    shops <- case w^.wShop of
        Just pid -> do
            let filtered = delete pid pids
                (rs', rpids) = randIds rs filtered (producerN-1) (consTrials-1)
            sRandoms .= rs'
            return (pid:rpids)
        Nothing -> do
            let (rs', rpids) = randIds rs pids producerN consTrials
            sRandoms .= rs'
            return rpids
    priced <- mapM getPrice shops
    let sorted = sortBy (compare `on` snd) priced
    (remain, visited) <- foldM buy (demand, []) sorted
    if not (null visited)
        then do
            let biggest = fst $ maximumBy (compare `on` snd) visited
            workers. ix wid .wShop .= Just biggest
        else workers. ix wid .wShop .= Nothing
    workers.ix wid.wSavings += (income + remain - demand)
    aNSales += demand - remain
  where
    getPrice :: Pid -> Simulation (Pid, Money)
    getPrice pid = do
        Just p <- use $ producers . at pid
        let price = p^.pPrice
        return (pid, price)
    buy :: (Money, [(Pid, Int)]) -> (Pid, Money) -> Simulation (Money, [(Pid, Int)])
    buy (demand, shops) (pid, price) = do 
        Just p <- use $ producers. at pid
        let wares = p ^. pGoods
        case wares of
            Nothing -> return (demand, shops)
            Just w -> if  demand > 0
                then do
                    let value = w*price
                        pSize = length (p^.pWorkers)
                    if value < demand 
                        then do -- case buy all
                            let p' = p & pGoods .~ Nothing
                                       & pCash +~ value
                                       & pNomSales +~ value
                            producers . ix pid .= p'
                            aRSales += w
                            return (demand-value, (pid, pSize):shops)
                        else do -- case exhaust demand
                            let p' = p & pGoods .~ Just (w - demand/price)
                                       & pCash +~ demand
                                       & pNomSales +~ demand 
                            producers . ix pid .= p
                            aRSales += demand/price
                            return (0, (pid, pSize):shops)
                else return (demand, shops)

------------------Repayment---------------------
moneyStep :: Simulation ()
moneyStep = do
    pids <- use producerIds
    bids <- use bankIds
    mapM_ payLoan pids 
    mapM_ banking bids

payLoan :: Pid -> Simulation ()
payLoan pid = do
    Just p <- use $ producers. at pid
    let cash = p^.pCash
        debt = p^.pDebt
    np <- case debt of
        Just (bid, loan) ->
            if loan > cash
                then do 
                    banks.ix bid.bCash += cash
                    let p' = p&pCash .~ 0
                              &pDebt .~ Just (bid, loan - cash)
                              &pProfit .~ 0 - (p^.pClosing)
                              &pClosing .~ 0
                    return p'
                else do
                    banks.ix bid.bCash += loan
                    let balance = cash - loan
                        p' = p&pCash -~ loan
                              &pDebt .~ Nothing
                              &pProfit .~ balance - (p^.pClosing)
                              &pClosing .~ balance
                    return p'
        Nothing -> do
            let p' = p&pProfit .~ cash - (p^.pClosing)
                      &pClosing .~ cash
            return p'
    producers.ix pid .= np

banking :: Bid -> Simulation ()
banking bid = do
    Just b <- use $ banks.at bid
    let b' = b&bCash -~ (b^.bDebt)
              &bDebt .~ 0
    banks.ix bid.=b'


---------------Global Information----------------
--Minimum wage?


