module Day where
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.IntMap as Map
import Data.List
import Debug.Trace
import System.Random
import Data.Random

import AgentTypes
import Simulation
import SimUtils

-- | Progress one day in simulation.
-- Households buy stuff.
-- Firms produce more stuff.
runDay :: Simulation ()
runDay = undefined


