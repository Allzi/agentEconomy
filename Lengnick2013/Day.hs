module Day where
import Control.Lens
import Control.Monad.State.Strict
import Data.Random
import Data.RVar

import AgentTypes
import Simulation
import SimUtils

-- | Progress one day in simulation.
-- Households buy stuff.
-- Firms produce more stuff.
runDays :: Simulation ()
runDays = do
    timer._3 .= 0
    loopDays
  where
    loopDays :: Simulation ()
    loopDays = do
        buyGoods
        produceGoods
        timer._3+=1
        d <- use $ timer._3
        when (d<daysInMonth) loopDays


buyGoods :: Simulation ()
buyGoods = do
    hids <- use householdIds
    shuffleds <- sampleRVar $ shuffleN householdN hids
    mapM_ makeVisits shuffleds
  where
    makeVisits :: Hid -> Simulation ()
    makeVisits hid = do
        Just h <- use $ households.at hid
        h' <- visitShops h (h^.hDDemand) (h^.hShops) shopN
        households.ix hid .= h'

visitShops :: Household -> Stuff -> [Fid] -> Int -> Simulation Household
visitShops h _ []    _  = return h -- no shops left
visitShops h _ _     0  = return h -- no shops left (?!)
visitShops h 0 _     _  = return h -- no demand left.
visitShops h d shops sn = do
    --choose random fid from list
    (fid, remainingShops) <- sampleRVar $ randomElementNR sn shops

    Just f <- use $ firms . at fid
    let p = f^.fPrice
        maxAffort = h^.hLiquity / p
        dem = min maxAffort d
        actDem = min (f^.fInventory) dem
        unsat = dem - actDem
    
    let f' = f&fInventory   -~ actDem
              &fMDemand     +~ dem
              &fLiquity     +~ actDem*p
    firms.ix fid .= f'
    
    let h' = h&hLiquity     -~ actDem*p
              &hUnsatDemand %~ addUnsat fid unsat
        unsatSmallEnough = unsat < (1-endShopTresh) * h^.hDDemand
    if unsatSmallEnough
        then return h'
        else visitShops h' unsat remainingShops (sn - 1)
  where
    addUnsat :: Fid -> Double -> [(Fid, Double)]-> [(Fid, Double)]
    addUnsat fid1 u xs = if u > 0 
        then go xs
        else xs
      where
        go [] = [(fid1, u)]
        go ((fid2, ud):ls) = if fid2 == fid1
            then (fid2, ud+u):ls
            else (fid2, ud):(go ls)


produceGoods :: Simulation ()
produceGoods = firms %= fmap produce
  where
    produce :: Firm -> Firm
    produce f = f&fInventory +~ fromIntegral (f^.fSize) * productivity
