{-# LANGUAGE PartialTypeSignatures #-}

-- | This 2D chart plots simple polygons onto a 2D surface. Each polygon has an associated stroke
-- and fill color.
--
-- This plot is a generalized variant of "FillBetween", where simple polygons, not just lines with
-- boundaries can be plotted.
--
-- My application is to draw contour plots.

module Graphics.Rendering.Chart.Plot.SimplePolygons where

import Control.Lens
import Data.Colour (Colour, black, opaque)
import Data.Default (Default)
import Graphics.Rendering.Chart (
  BackendProgram,
  FillStyle,
  Plot (..),
  PointMapFn,
  ToPlot (..),
  alignFillPoints,
  fill_color,
  mapXY,
  withFillStyle, LineStyle, withLineStyle, strokePointPath, solidFillStyle, Rect, fillPath, rectPath,
 )
import Graphics.Rendering.Chart.Drawing (fillPointPath)
import Graphics.Rendering.Chart.Easy (Default (..))
import Data.Colour.SRGB (sRGB)

-- | Draw simple polygons onto a 2D surface.
--
-- TODO Currently, I use a "fill function", instead of attaching this information to the "polygons"
-- element.

data PlotSimplePolygons x y = PlotSimplePolygons
  { _plot_polygons_title :: String
  , _plot_polygons_line :: Maybe LineStyle
  , _plot_polygons_fill :: FillStyle
  , _plot_polygons_polygons :: [[(x,y)]]
  }
makeLenses ''PlotSimplePolygons

instance Default (PlotSimplePolygons x y) where
  def = PlotSimplePolygons
          { _plot_polygons_title = ""
          , _plot_polygons_line = Nothing
          , _plot_polygons_fill = solidFillStyle (opaque $ sRGB 0.5 0.5 1.0)
          , _plot_polygons_polygons = []
          }

instance Graphics.Rendering.Chart.ToPlot PlotSimplePolygons where
  {-# Inlinable toPlot #-}
  toPlot p = Graphics.Rendering.Chart.Plot
    { _plot_render = renderSimplePolygons p
    , _plot_legend = [(_plot_polygons_title p, renderPlotLegendPolygons p)]
    , _plot_all_points = plotAllPolygons p
    }

renderSimplePolygons :: PlotSimplePolygons x y -> Graphics.Rendering.Chart.PointMapFn x y -> Graphics.Rendering.Chart.BackendProgram ()
{-# Inlinable renderSimplePolygons #-}
renderSimplePolygons p pmap = mapM_ go (_plot_polygons_polygons p)
  where
    go xys = withFillStyle (_plot_polygons_fill p) $ do
      ps <- Graphics.Rendering.Chart.alignFillPoints $ [ mapXY pmap (x,y) | (x,y) <- xys ]
      fillPointPath ps
      case _plot_polygons_line p of
        Nothing -> pure ()
        Just linestyle -> withLineStyle linestyle $ strokePointPath ps

plotAllPolygons :: PlotSimplePolygons x y -> ([x],[y])
{-# Inlinable plotAllPolygons #-}
plotAllPolygons p = ( [ x | xys <- _plot_polygons_polygons p, (x,_) <- xys ]
                    , [ y | xys <- _plot_polygons_polygons p, (_,y) <- xys ]
                    )

renderPlotLegendPolygons :: PlotSimplePolygons x y -> Rect -> BackendProgram ()
{-# Inlinable renderPlotLegendPolygons #-}
renderPlotLegendPolygons p r =
  withFillStyle (_plot_polygons_fill p) $ fillPath (rectPath r)

