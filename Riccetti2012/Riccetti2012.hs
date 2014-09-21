{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad.State.Strict
import Control.Lens
import System.Directory
import Control.DeepSeq
import System.Random

import Simulation
import SimSteps
import SimIO
import SimUtils


main = do
    simData <- loop $ startSim (randoms (mkStdGen seed))
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

