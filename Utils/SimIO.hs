module SimIO where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import Data.List

import SimUtils


printData :: SimData -> IO()
printData ((a,b):d) = do
    print $ a ++ (show b)
    printData d
printData _ = return ()

--------------------Chart Making-----------------
type PDat = [(String, [(Double,Double)])]

toPlotData :: [SimData] -> [(String, PDat)]
toPlotData d = collectPlots labeled
  where
    d' = transpose d
    labeled = fmap labelData d'
    collectPlots :: PDat -> [(String, PDat)]
    collectPlots dat = fmap (\a -> (fst a, [a])) dat

--gives label and x values:
labelData :: [(String, Double)] -> (String, [(Double, Double)])
labelData d = (l, points)
  where
    (l, _) = head d
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
dataToChart (n, d) = renderableToFile def (n ++ ".png") (chart n d) 

dataToCharts :: [SimData] -> IO()
dataToCharts sd = mapM_ dataToChart d
  where 
    d = toPlotData sd
