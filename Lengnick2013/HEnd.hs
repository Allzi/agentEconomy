{-# LANGUAGE MultiWayIf #-}
module HEnd where
import Prelude hiding (sum, foldl, any, mapM_)

import Control.Lens
import Control.Monad.State.Strict hiding (mapM_)
import Data.Foldable
import Data.Random

import AgentTypes
import Simulation
import SimUtils


incomeGetting :: Simulation ()
incomeGetting = do
    households <$=> getWageInc
    calcWealth
    w <- use sHousWealth
    d <- use sDividends
    households %= fmap (getDivInc w d)
  where
    calcWealth :: Simulation ()
    calcWealth = do
        hs <- use households
        sHousWealth .= (sum.fmap _hLiquity) hs

getWageInc :: Household -> Simulation Household
getWageInc h = case h^.hEmployer of
    Nothing -> return $ h&hWage .~ Nothing
    Just fid -> do
        Just f <- use $ firms.at fid
        let w = f^.fActualWage
        return $ h&hWage    .~ Just w
                  &hLiquity +~ w

getDivInc :: Money -> Money -> Household -> Household
getDivInc w d h = h&hLiquity +~ ((h^.hLiquity) * d) / w

adjustResWages :: Simulation ()
adjustResWages = households %= fmap adjustW
  where
    adjustW :: Household -> Household
    adjustW h = case h^.hWage of
        Nothing -> h&hResWage *~ (1 - resWageDrop)
        Just w -> if w > h^.hResWage
            then h&hResWage .~ w
            else h
    
