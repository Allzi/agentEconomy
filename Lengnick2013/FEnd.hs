{-# LANGUAGE MultiWayIf #-}
module FEnd where
import Prelude hiding (sum, foldl, any, mapM_)

import Control.Lens
import Control.Monad.State.Strict hiding (mapM_)
import Data.Foldable
import Data.Random

import AgentTypes
import Simulation
import SimUtils


accountingStep :: Simulation ()
accountingStep = do
    firms %= fmap payWages
    sDividends .= 0 
    firms <$=> payDividends

payWages :: Firm -> Firm
payWages f = f'
  where
    fSze = fromIntegral (f^.fSize)
    wageBill = f^.fWageRate * fSze
    f' = if wageBill > f^.fLiquity
        then f&fActualWage .~ (f^.fLiquity)/fSze
             -- &fWageRate .~ (f^.fLiquity)/fSze
              &fLiquity  .~ 0
        else f&fLiquity -~ wageBill
              &fActualWage .~ (f^.fWageRate)
    

payDividends :: Firm -> Simulation Firm
payDividends f = do
    let fSze = fromIntegral (f^.fSize)
        buffer = fSze * (f^.fWageRate) * mBufferMult
        div = f^.fLiquity - buffer
    if div < 0
        then return f
        else do
            sDividends += div
            return $ f&fLiquity -~ div
