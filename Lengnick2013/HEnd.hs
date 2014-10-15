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

-- | Household collects its wage income and updates the reservation wage.
getWageInc :: Household -> Simulation Household
getWageInc h = case h^.hEmployer of
    Nothing -> return $ h&hResWage *~ (1 - resWageDrop)                       
    Just fid -> do
        Just f <- use $ firms.at fid
        let w = f^.fWageRate
        return $ h&hResWage %~ max w
                  &hLiquity +~ w

getDivInc :: Money -> Money -> Household -> Household
getDivInc w d h = h&hLiquity +~ ((h^.hLiquity) * d) / w
