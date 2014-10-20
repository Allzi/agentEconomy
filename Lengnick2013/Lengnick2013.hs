module Main where
import System.Directory

import SimSteps
import SimIO

main :: IO ()
main = do
    let simData = simulateSeed 1
    mapM_ printData simData
    
    createDirectoryIfMissing False "Output"
    setCurrentDirectory "Output"
    dataToCharts simData


