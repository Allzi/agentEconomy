{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad.State
import Control.Lens
import Simulation



main = do
   let sim = startSim
   loop startSim
   print "baa"

loop :: SimState -> IO ()
loop sim =
    when (sim^.timer < duration) $ do
        let (_, sim') = runState simStep sim
        loop sim'
