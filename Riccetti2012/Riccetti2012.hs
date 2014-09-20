{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad.State.Strict
import Control.Lens
import System.Directory

import Simulation
import SimSteps
import SimIO
import SimUtils


main = do
    let sim = startSim
    simData <- loop startSim
    mapM printData simData
    createDirectoryIfMissing False "Output"
    setCurrentDirectory "Output"
    dataToCharts simData

loop :: SimState -> IO ([SimData])
loop sim =
    if (sim^.timer < duration) 
        then do
            let (dPoint, sim') = runState simStep sim
            simData <- loop sim'
            return (dPoint:simData)
        else return []

