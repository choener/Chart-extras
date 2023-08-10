
-- | Plots a step function. The half-point between two steps is where the actual transition to the
-- next step happens. For each step, the actual @(x,y)@ value is the midpoint of the step.

module Graphics.Rendering.Chart.Plot.Step where

import Graphics.Rendering.Chart
import Data.Default.Class (Default(..))
import Data.Colour (opaque)
import Data.Colour.Names (blue, black)


-- |
--
-- TODO Consider a flag for the first/last point to go half a step beyond the data.

data PlotStep x y = PlotStep
  { _plot_step_title :: String
  , _plot_step_style :: LineStyle
  , _plot_step_values :: [(x,y)]
  , _plot_step_point_style :: Maybe PointStyle -- would add a point where exactly the position is
  }

instance Default (PlotStep x y) where
  def = PlotStep
    { _plot_step_title = ""
    , _plot_step_style = defaultPlotStepStyle
    , _plot_step_values = []
    , _plot_step_point_style = Just def
    }

instance ToPlot PlotStep where
  toPlot p = Plot
    { _plot_render = undefined
    , _plot_legend = [(_plot_step_title p, renderPlotLegendStep p)]
    , _plot_all_points = undefined
    }

renderPlotLegendStep :: PlotStep x y -> Rect -> BackendProgram ()
renderPlotLegendStep p (Rect p1 p2) = line >> point where
  line = withLineStyle (_plot_step_style p) $ do
    let y = (p_y p1 + p_y p2) / 2
    ps <- alignStrokePoints [Point (p_x p1) y, Point (p_x p2) y]
    strokePointPath ps
  point = case _plot_step_point_style p of
    Just ps -> do
      let y = (p_y p1 + p_y p2)/2
      drawPoint ps (Point (p_x p1)              y)
      drawPoint ps (Point ((p_x p1 + p_x p2)/2) y)
      drawPoint ps (Point (p_x p2)              y)
    Nothing -> pure ()

defaultPlotStepStyle :: LineStyle
defaultPlotStepStyle = (solidLine 1 $ opaque black)
  { _line_cap  = LineCapRound
  , _line_join = LineJoinRound
  }

{-
instance ToPlot PlotLines where
    toPlot p = Plot {
        _plot_render     = renderPlotLines p,
        _plot_legend     = [(_plot_lines_title p, renderPlotLegendLines p)],
        _plot_all_points = ( map fst pts ++ xs, map snd pts ++ ys )
    }
      where
        pts = concat (_plot_lines_values p)
        xs = [ x | (LValue x,_) <- concat (_plot_lines_limit_values p)]
        ys = [ y | (_,LValue y) <- concat (_plot_lines_limit_values p)]

renderPlotLines :: PlotLines x y -> PointMapFn x y -> BackendProgram ()
renderPlotLines p pmap = 
  withLineStyle (_plot_lines_style p) $ do
    mapM_ (drawLines (mapXY pmap)) (_plot_lines_values p)
    mapM_ (drawLines pmap) (_plot_lines_limit_values p)
  where
    drawLines mapfn pts = alignStrokePoints (map mapfn pts) >>= strokePointPath 


defaultPlotLineStyle :: LineStyle
defaultPlotLineStyle = (solidLine 1 $ opaque blue){
     _line_cap  = LineCapRound,
     _line_join = LineJoinRound
 }

instance Default (PlotLines x y) where
  def = PlotLines 
    { _plot_lines_title        = ""
    , _plot_lines_style        = defaultPlotLineStyle
    , _plot_lines_values       = []
    , _plot_lines_limit_values = []
    }

-- | Helper function to plot a single horizontal line.
hlinePlot :: String -> LineStyle -> b -> Plot a b
hlinePlot t ls v = toPlot def {
    _plot_lines_title        = t,
    _plot_lines_style        = ls,
    _plot_lines_limit_values = [[(LMin, LValue v),(LMax, LValue v)]]
    }

-- | Helper function to plot a single vertical line.
vlinePlot :: String -> LineStyle -> a -> Plot a b
vlinePlot t ls v = toPlot def {
    _plot_lines_title        = t,
    _plot_lines_style        = ls,
    _plot_lines_limit_values = [[(LValue v,LMin),(LValue v,LMax)]]
    }

-}
