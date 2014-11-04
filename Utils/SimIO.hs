module SimIO where
import Prelude hiding (sequence_, sequence, mapM_)
import Control.Lens
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Foldable

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Csv hiding ((.=))

import SimUtils
-- * Chart Making

-- | Makes charts from all the tables in SimData.
dataToCharts :: SimData -> IO ()
dataToCharts sd = sequence_ $ M.mapWithKey makeChart sd
  where
    makeChart :: String -> SimTable -> IO ()
    makeChart t tab = toFile def (t ++ ".png") $ do
        layout_title .= t
        sequence_ $ M.mapWithKey plotData tab
    plotData key d = plot(line key labeled)
      where
        labeled :: [[(Double, Double)]]
        labeled = [zip [1.0,2.0..] d]


-- * Writing CSV Files

simDataToCSV :: SimData -> IO ()
simDataToCSV sd =  mapM_ go listed
  where
    listed = M.toList sd
    go (a, b) = simTableToCSV a b



simTableToCSV :: String -> SimTable -> IO ()
simTableToCSV fname st = do
    let listed = M.toList st
        recordList = L.transpose (fmap nameData listed)
        headerVector = V.fromList (fmap (\(a, _) -> toField a) listed)
        encoded = encodeByName headerVector (fmap namedRecord recordList)
    BL.writeFile (fname ++ ".csv") encoded
  where
    nameData :: (String, [Double]) -> [(B.ByteString, B.ByteString)]
    nameData (h, bs) = namedTuples
      where
        nameString = toField h
        namedTuples = fmap (\c -> namedField nameString c) bs
