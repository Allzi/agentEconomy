{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad.State
import Control.Lens
import System.Directory
import Control.DeepSeq

import Simulation
import SimSteps
import SimIO
import SimUtils

type PDat = [(String, [(Double,Double)])]

main = do
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
            simData <- dPoint `deepseq` loop sim'
            return (dPoint:simData)
        else return []

