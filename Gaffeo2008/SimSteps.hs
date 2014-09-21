module SimSteps where
import Control.Lens
import Control.Monad.State
import qualified Data.IntMap.Strict as Map
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
    devStep
    planStep
    laborStep
    creditStep
    productionStep
    consStep
    moneyStep
    solvensyStep
    timer += 1
    -- Comes from Simulation.hs  
    collectData

--------------Generic stuff-------------------
simplePStep :: ([Double] -> Producer -> ([Double], Producer)) -> Simulation()
simplePStep f = do
    prs <- use producers
    newProducers <- randSim (\rs -> Map.mapAccum f rs prs)
    producers .= newProducers

------------------New productivities---------------------
devStep :: Simulation ()
devStep = simplePStep develop

develop :: [Double] -> Producer -> ([Double] ,Producer)
develop (r:rs) pr = (rs, nprod)
  where
    nprod = pr&pProductivity +~ exponNum r mu
           --   &pCash -~ contr
    mu = 
        if contr > 0
            then contr / pr ^. pNomSales
            else 0
    contr = 
        if pr^.pProfit > 0
            then pr^.pProfit * rdinv1
            else 0

-- takes a random number in [0,1] and mean
exponNum :: Double -> Double -> Double
exponNum r mu = (-mu) * log r

----------------Plans for production-------------------
planStep :: Simulation ()
planStep = do
    avgP <- use avgPrice
    simplePStep (updatePlan avgP)

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
    mapMp planHiring
    -- Send applications
    mapMw searchJob
    -- Send offers
    mapMp sendOffers
    -- Sign contracts
    mapMw signContract
   

planHiring :: Producer -> Simulation Producer
planHiring p = do
    let sorted = sortBy (compare `on` snd) $ p^.pWorkers
    p' <- randSim (\rs -> calcDemand rs p)
    return $ p'&pApplicants.~[]
               &pWorkers .~ sorted

searchJob :: Worker -> Simulation Worker
searchJob w = do
    pids <- use producerIds
    apps <- case w^.wEmployer of
        Just pid -> do
            Just emp <- use $ producers . at pid
            if not $ null (emp^.pWorkers)
                then randApps (delete pid pids) (producerN-1) (laborTrials-1)
                else randApps pids producerN laborTrials
        Nothing -> randApps pids producerN laborTrials
    mapM_ send apps
    return $ w&wOffers.~[]
              &wEmployer .~ Nothing
  where
    randApps ids n1 n2 =
        randSim (\rs -> randIds rs ids n1 n2)
    send pid =
        producers.ix pid.pApplicants %= ((w^.wID):)

sendOffers :: Producer -> Simulation Producer
sendOffers p = do
    let apps = p^.pApplicants
    shuffled <- shuffle apps (length apps)
    let all = (p^.pWorkers) ++ (fmap (\a -> (a, p^.pEntrantWage)) shuffled)
        receirvers = take (p^.pLDemand) all
    mapM_ send receirvers
    return $ p&pWorkers .~ []
  where
    pid = p^.pID
    send (wid, wage) = do
        workers.ix wid.wOffers %= ((pid, wage):)

signContract :: Worker -> Simulation Worker
signContract w = do
    let best = choose $ w^.wOffers
    sign best
  where
    choose a = 
        if not $ null a
            then Just $ maximumBy (compare `on` snd) a
            else Nothing
    sign :: Maybe (Pid, Money) -> Simulation Worker
    sign (Just (pid, wage)) = do
        producers.ix pid.pWorkers %= ((w^.wID, wage):)
        producers.ix pid.pCash -= wage
        return $ w&wIncome .~ wage
                  &wEmployer .~ Just pid
    sign Nothing = do
        return $ w&wIncome .~ 0
                  &wEmployer .~ Nothing


calcDemand :: [Double] -> Producer -> ([Double], Producer)
calcDemand (r:rs) p = (rs, p')
  where 
    ldemand = ceiling (p^.pQuantity / p^.pProductivity)
    wagemult = 
        if ldemand > length (p^.pWorkers)
            then (1+r*wageGrowth)
            else 1
    p' = p&pLDemand .~ ldemand
          &pEntrantWage*~ wagemult

---------------Credit markets---------------
creditStep :: Simulation ()
creditStep = do
    pids <- use producerIds
    shuffled <- shuffle pids producerN
    mapM_ getFinance pids
    rate <- use iRate
    banks %= fmap (bankFinance rate)
  
getFinance :: Pid -> Simulation ()
getFinance pid = do
    Just p <- use $ producers . at pid
    bIds <- use bankIds
    let need = -(p^.pCash)
    when (need>0) $ do
        -- get random banks
        bs <- randSim (\rs -> randIds rs bIds bankN creditTrials)
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
            rate = cRate * (1 + r * bankCosts * mUp leverage)
        sRandoms .= rs
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
    pids <- use producerIds
    let income = w^.wIncome
        demand = if income>0 
            then income * mrC1
            else (w^.wSavings) * mrC2
    shops <- case w^.wShop of
        Just pid -> do
            let filtered = delete pid pids
            rpids <- randSim (\rs -> randIds rs filtered (producerN-1) (consTrials-1))
            return (pid:rpids)
        Nothing ->
            randSim (\rs -> randIds rs pids producerN consTrials)
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
                            producers . ix pid .= p'
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
randEntrant selection num (r:rs) old = 
    if (num<1) 
        then error "Error: no solvent firms left!"
        else (rs, a')
  where
    a = selection !! floor (fromIntegral num *r)
    a' = (makeEntrant a)&idLens.~(old^.idLens)



---------------Global Information----------------
infoStep :: Simulation ()
infoStep = do
    ps <- use $ producers
    let avgP = (Map.foldl (\acc p -> acc+(p^.pPrice)) 0 ps)/fromIntegral producerN
    avgPrice .= avgP

