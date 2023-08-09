
-- | Plots a step function. The half-point between two steps is where the actual transition to the
-- next step happens. For each step, the actual @(x,y)@ value is the midpoint of the step.

module Graphics.Rendering.Chart.Plot.Step where

import Graphics.Rendering.Chart (LineStyle, ToPlot (toPlot))
import Data.Default.Class (Default(..))


-- |
--
-- TODO Consider a flag for the first/last point to go half a step beyond the data.

data PlotStep x y = PlotStep
  { _plot_step_title :: String
  , _plot_step_style :: LineStyle
  , _plot_step_values :: [(x,y)]
  }

instance Default (PlotStep x y) where
  def = PlotStep
    { _plot_step_title = ""
    , _plot_step_style = def
    , _plot_step_values = []
    }

instance ToPlot PlotStep where
  toPlot = undefined

