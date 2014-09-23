module Main where
import Control.Lens
import Control.Monad.State.Strict
import System.Directory
import Control.DeepSeq

import Data.Random
import System.Random

import SimSteps
import SimIO


main = do
    let simData = simulateSeed 1
    mapM printData simData
    
    createDirectoryIfMissing False "Output"
    setCurrentDirectory "Output"
    dataToCharts simData


