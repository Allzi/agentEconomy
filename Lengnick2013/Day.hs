module Day where
import Control.Lens
import Control.Monad.State.Strict
import Data.Random

import AgentTypes
import Simulation

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
    shuffleds <- lift $ shuffleN householdN hids
    mapM_ makeVisits shuffleds
  where
    makeVisits :: Hid -> Simulation ()
    makeVisits hid = do
        Just h <- use $ households.at hid
        roShops <- lift $ shuffleN shopN (h^.hShops)
        h' <- visitShops h (h^.hDDemand) roShops
        households.ix hid .= h'

visitShops :: Household -> Stuff -> [(Fid, Money)] -> Simulation Household
visitShops h _ [] = return h
visitShops h 0 _ = return h
visitShops h d ((fid, p):fs) = do
    Just f <- use $ firms . at fid
    let maxAffort = h^.hLiquity / p
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
        else visitShops h' unsat fs 
  where
    addUnsat :: Fid -> Double -> [(Fid, Double)]-> [(Fid, Double)]
    addUnsat fid1 = go
      where
        go 0 ls = ls
        go u [] = [(fid1, u)]
        go u ((fid2, ud):ls) = if fid2 == fid1
            then (fid2, ud+d):ls
            else (fid2, ud):(addUnsat fid1 u ls)



produceGoods :: Simulation ()
produceGoods = firms %= fmap produce
  where
    produce :: Firm -> Firm
    produce f = f&fInventory +~ fromIntegral (f^.fSize) * productivity
