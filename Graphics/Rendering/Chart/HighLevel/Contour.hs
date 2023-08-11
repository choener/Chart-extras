
-- | This module provides high-level functionality to create contour plots.
--
-- It uses the 'PlotSimplePolygons' style for plotting, and hgeometry under the hood to generate the
-- polygons themselves.

module Graphics.Rendering.Chart.HighLevel.Contour where

import Algorithms.Geometry.DelaunayTriangulation.Types (Triangulation, edgesAsPoints, toPlaneGraph)
import Control.Lens (view, (&), (.~), makeLenses, _3)
import Control.Monad (foldM)
import Data.Ext (core, (:+) (..))
import Data.Geometry (Point (..))
import Data.Geometry.Polygon (SimplePolygon, toVector)
import Data.PlaneGraph (FaceId', faceBoundary, internalFaces)
import Data.Set (Set)
import Data.Vector (Vector)
import Debug.Trace (traceShow)
import Numeric.Log
import qualified Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer as DC
import qualified Algorithms.Geometry.DelaunayTriangulation.Naive as NA
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V
import Graphics.Rendering.Chart (Plot (..), FillStyle, ToPlot (toPlot), LineStyle, joinPlot)
import Data.Default (Default (def))
import Data.Colour (opaque, transparent)
import Data.Colour.SRGB (sRGB)
import Data.Foldable (Foldable(foldl'))
import Data.List ( partition, foldl1' )
import Graphics.Rendering.Chart.Plot.SimplePolygons
import Graphics.Rendering.Chart.Drawing (solidFillStyle)



-- This generates a 2D chart that uses a Delaunay triangulation to plot approximate densities.
--
-- 1. Each sample point in 2D @Point 2 r@ is combined with an evaluated density @Log Double@ into:
-- @Point 2 r :+ Log Double@
-- 2. The @Triangulation (Log Double) r@ is calculated.
-- 3. From the triangulation, all faces are extracted, together with the lowest @Log Double@ over
-- all vertices belonging to the face.
-- 4. Given a map @low, high :: Log Double -> Color@, find the color for each face.
-- 5. Draw each face together with the color.
--
-- a. The Map could, for example, partition the (ordered by likelihood) weighted samples equally.
-- This way, each of (say) 10 colors defines 10% of the probability mass.
--
-- TODO Rename to sth. like "Triangulation", since this plot structure mainly shall do that type of
-- plotting.
--
-- TODO Once a ladder has been established, I should be able to select just the points closed to
-- each boundary and render with those ...
--
-- TODO Alternatively, consider drawing convex hulls -- but for multimodal distributions I have to
-- figure out how to determine the modes -- that should be a graph problem, where edges are deleted
-- that connect points with too small or too large values?
--
-- TODO When using the "ladder" approach, for each ladder, find the connected components and only
-- draw the polygon made up by the outer edges. This will save on drawing time, but might not be
-- worth it unless huge number of polygons are used.

data PlotDelaunayContours p x y = PlotDelaunayContours
  { _plot_delaunaycontours_title :: String
  , _plot_delaunaycontours_contours :: [(p,p,String,FillStyle)]
  , _plot_delaunaycontours_points :: [(x,y,p)]
  , _plot_delaunaycontours_line :: Maybe LineStyle
  , _plot_delaunaycontours_fill :: Maybe FillStyle
  }
makeLenses ''PlotDelaunayContours

instance Default (PlotDelaunayContours p x y) where
  def = PlotDelaunayContours
          { _plot_delaunaycontours_title = ""
          , _plot_delaunaycontours_contours = []
          , _plot_delaunaycontours_points = []
          , _plot_delaunaycontours_line = Just def
          , _plot_delaunaycontours_fill = Nothing
          }

-- |
--
-- TODO each of the contours has limits @[lower,upper)@, a title, and fill style. Collect the points
-- based on the given contours, and render. All points not within the given contours are rendered
-- using the default style, if set.
--
-- TODO Modify the caption of the resulting Plot to show a very compressed view, instead of
-- individual legends. Should be possible by overriding the legend part.

plotDelaunayContours :: (Real x, Real y, Fractional x, Fractional y, Ord p) => PlotDelaunayContours p x y -> Plot x y
plotDelaunayContours p = foldl1' joinPlot (remplot:dstplots)
  where
    remplot = toPlot $ def
      & plot_polygons_title .~ _plot_delaunaycontours_title p ++ "Contours:"
      & plot_polygons_polygons .~ map (\xs -> [ (x,y) | (x,y,_) <- xs]) remainder
      & plot_polygons_fill .~ maybe (solidFillStyle transparent) id (_plot_delaunaycontours_fill p)
      & plot_polygons_line .~ _plot_delaunaycontours_line p
    dstplots = [ toPlot $ def & plot_polygons_title .~ title
                              & plot_polygons_fill .~ style
                              & plot_polygons_polygons .~ map (\ps -> [(x,y) | (x,y,_) <- ps]) tri
               | (low,high,title,style,tri) <- dst ]
    (dst,remainder) = go (_plot_delaunaycontours_contours p) [] tris
    -- Repeatedly partition the set of points, until no more contours are available, then return
    -- the individual contours, and the remainder to draw via the default function. This has a
    -- worst case running time of @p*c@ where @p@ is the the number of points, and @c@ the number
    -- of contours.
    go [] dst ts = (dst, ts)
    go ((low,high,title,style):contours) dst ts =
      let (yes,no) = partition (\ps -> let p = minimum (map (view _3) ps) in low <= p && p < high) ts
      in  go contours ((low,high,title,style,yes):dst) no
    tris = genTriangles $ _plot_delaunaycontours_points p

--  toPlot p = Graphics.Rendering.Chart.Plot
--    { _plot_render = renderSimplePolygons p
--    , _plot_legend = [(_plot_polygons_title p, renderPlotLegendPolygons p)]
--    , _plot_all_points = plotAllPolygons p
--    }

-- | Given a triangulation, will return all the triangular faces. Note that we leave @p@ alone here,
-- which means in @p@ we can have the whole sample information. A small mapping function will then
-- convert the inner vector into a format suitable for @Chart@ing.
--
-- BUG possibly use 'faces', not 'internalFaces'.

triangularFaces :: forall p r . (Ord r, Num r) => Triangulation p r -> Vector (Vector (Point 2 r :+ p))
{-# Inlinable triangularFaces #-}
triangularFaces tri = V.map toVector ps
  where
    pg = toPlaneGraph tri
    --fs :: Vector (FaceId' s, ())
    fs = internalFaces pg
    ps :: Vector (SimplePolygon p r)
    ps = V.map (\(fid,_) -> view core $ faceBoundary fid pg) fs

genTriangles :: (Real x, Real y, Fractional x, Fractional y) => [(x,y,p)] -> [[(x,y,p)]]
{-# Inlinable genTriangles #-}
genTriangles xyp = V.toList $ V.map (V.toList . V.map (\(Point2 x y :+ p) -> (fromRational x,fromRational y,p))) faces
  where
    triangulation = DC.delaunayTriangulation $ NE.fromList [ Point2 (toRational x) (toRational y) :+ p | (x,y,p) <- xyp ]
    faces = triangularFaces triangulation

