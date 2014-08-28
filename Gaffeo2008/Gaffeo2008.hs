{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad.State
import Control.Lens

import Simulation
import SimSteps



main = do
   let sim = startSim
   simData <- loop startSim
   mapM printData simData

loop :: SimState -> IO ([SimData])
loop sim =
    if (sim^.timer < duration) 
        then do
            let (dPoint, sim') = runState simStep sim
            simData <- loop sim'
            return (dPoint:simData)
        else return []

printData :: SimData -> IO()
printData sdata = do
    print $ (sdata^.pLevel._1) ++ (show (sdata ^. pLevel._2))
