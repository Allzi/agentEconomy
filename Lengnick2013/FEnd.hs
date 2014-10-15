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
    liquity = f^.fLiquity
    f' = if wageBill > liquity && f^.fSize > 0
        then do
            f&fWageRate .~ liquity/fSze
             &fLiquity  .~ 0
        else do
            f&fLiquity -~ wageBill
    

payDividends :: Firm -> Simulation Firm
payDividends f = if div > 0
    then do
        sDividends += div
        return $ f&fLiquity -~ div
    else return f
  where 
    fSze = fromIntegral (f^.fSize)
    buffer = fSze * (f^.fWageRate) * mBufferMult
    div = f^.fLiquity - buffer

