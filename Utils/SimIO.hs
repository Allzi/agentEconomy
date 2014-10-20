module SimIO where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens
import qualified Data.Map as M

import SimUtils

--------------------Chart Making-----------------
type PDat = [(String, [(Double,Double)])]

toPlotData :: SimData -> [(String, PDat)]
toPlotData sd = collectPlots labeledList
  where
    labeledList = (M.toList.fmap (zip [1, 2..])) sd
    collectPlots :: PDat -> [(String, PDat)]
    collectPlots dat = fmap (\a -> (fst a, [a])) dat



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

dataToCharts :: SimData -> IO()
dataToCharts sd = mapM_ dataToChart d
  where 
    d = toPlotData sd
