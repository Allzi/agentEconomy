module Main where
import System.Directory

import SimSteps
import SimIO

main :: IO ()
main = do
    let simData = simulateSeed 1
    
    createDirectoryIfMissing False "Output"
    setCurrentDirectory "Output"
    dataToCharts simData
    simDataToCSV simData
    print "Simulation done."


