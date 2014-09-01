{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad.State
import Control.Lens
import Data.List

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo

import Simulation
import SimSteps


type PDat = [(String, [(Double,Double)])]

main = do
   let sim = startSim
   simData <- loop startSim
   mapM printData simData
   dataToCharts simData

loop :: SimState -> IO ([SimData])
loop sim =
    if (sim^.timer < duration) 
        then do
            let (dPoint, sim') = runState simStep sim
            simData <- loop sim'
            return (dPoint:simData)
        else return []

printData :: SimData -> IO()
printData ((a,b):d) = do
    print $ a ++ (show b)
    printData d
printData _ = return ()




--------------------Chart Making-----------------
toPlotData :: [SimData] -> [(String, PDat)]
toPlotData d = collectPlots labeled
  where
    d' = transpose d
    labeled = fmap labelData d'
    collectPlots :: PDat -> [(String, PDat)]
    collectPlots d = fmap (\a -> (fst a, [a])) d

--gives label and x values:
labelData :: [(String, Double)] -> (String, [(Double, Double)])
labelData d = (label, points)
  where
    (label, _) = head d
    points = zip [1,2..] (fmap (\(_,a) -> a) d)


chart :: String -> PDat -> Renderable ()
chart title ls = toRenderable layout
  where
    addLine (t, p) = plot_lines_values .~ [p]
                   $ plot_lines_style  . line_color .~ opaque blue
                   $ plot_lines_title .~ t
                   $ def

    lineFolder acc l = (toPlot (addLine l)):acc

    layout = layout_title .~ title
           $ layout_plots .~ foldl lineFolder [] ls
           $ def

dataToChart :: (String, PDat) -> IO (PickFn ())
dataToChart (n, d) = renderableToFile def (chart n d) (n++".png")

dataToCharts :: [SimData] -> IO()
dataToCharts sd = mapM_ dataToChart d
  where 
    d = toPlotData sd
    
