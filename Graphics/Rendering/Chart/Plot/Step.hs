
-- | Plots a step function. The half-point between two steps is where the actual transition to the
-- next step happens. For each step, the actual @(x,y)@ value is the midpoint of the step.

module Graphics.Rendering.Chart.Plot.Step where

import Graphics.Rendering.Chart
import Data.Default.Class (Default(..))
import Data.Colour (opaque)
import Data.Colour.Names (blue, black)
import Control.Monad (unless)


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
    { _plot_render = renderPlotStep p
    , _plot_legend = [(_plot_step_title p, renderPlotLegendStep p)]
    , _plot_all_points = plotAllPointsStep p
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

plotAllPointsStep :: PlotStep x y -> ([x],[y])
plotAllPointsStep = unzip . _plot_step_values

-- |
--
-- .x.v   v.x.
--    ^.x.^
--
-- TODO Include an optimization that removes consecutive points with same y-axis position.
--
-- TODO need to generate points first, so that "double"'s are available; however will this map
-- correctly in log-scale? I leave this for now to ponder.

renderPlotStep :: PlotStep x y -> PointMapFn x y -> BackendProgram ()
renderPlotStep p pmap = do
  unless (null ps) . withLineStyle (_plot_step_style p) $ do
    alignStrokePoints (go (let Point px py = head ps in Point (px-0.5) py) ps) >>= strokePointPath
  unless (null ps) $ case _plot_step_point_style p of
    Just sty -> mapM_ (drawPoint sty) ps
    Nothing -> pure ()
  where
    go (Point px py) [] = [Point (px+0.5) py]
    go (Point px py) (Point hx hy : next)
      = (Point ((px+hx)/2) py)
      : (Point ((px+hx)/2) hy)
      : (Point hx        hy)
      : go (Point hx hy) next
    ps = map (mapXY pmap) $ _plot_step_values p

