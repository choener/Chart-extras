{-# LANGUAGE PartialTypeSignatures #-}

-- | This 2D chart plots simple polygons onto a 2D surface. Each polygon has an associated stroke
-- and fill color.



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

module Graphics.Rendering.Chart.Plot.SimplePolygons where

import Algorithms.Geometry.DelaunayTriangulation.Types (Triangulation, edgesAsPoints, toPlaneGraph)
import Control.Lens
import Control.Lens (view)
import Control.Monad (foldM)
import Data.Colour (Colour, black, opaque)
import Data.Default (Default)
import Data.Ext (core, (:+) (..))
import Data.Geometry (Point (..))
import Data.Geometry.Polygon (SimplePolygon, toVector)
import Data.PlaneGraph (FaceId', faceBoundary, internalFaces)
import Data.Set (Set)
import Data.Vector (Vector)
import Debug.Trace (traceShow)
import qualified Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer as DC
import qualified Algorithms.Geometry.DelaunayTriangulation.Naive as NA
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Vector as V
import Graphics.Rendering.Chart (
  BackendProgram,
  FillStyle,
  Plot (..),
  PointMapFn,
  ToPlot (..),
  alignFillPoints,
  fill_color,
  mapXY,
  withFillStyle,
 )
import Graphics.Rendering.Chart.Drawing (fillPointPath)
import Graphics.Rendering.Chart.Easy (Default (..))
import Numeric.Log

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

-- | Draw simple polygons onto a 2D surface.
--
-- TODO Currently, I use a "fill function", instead of attaching this information to the "polygons"
-- element.

data PlotSimplePolygons p x y = PlotSimplePolygons
  { _plot_polygons_title :: String
  , _plot_polygons_polygons :: [[(x,y,p)]]
  , _plot_polygons_fillFun :: [(x,y,p)] -> FillStyle
    -- ^ Given the @p@ value, construct a fill colour.
  }
makeLenses ''PlotSimplePolygons

instance Default (PlotSimplePolygons p x y) where
  def = PlotSimplePolygons "" [] (const $ def & fill_color .~ opaque black)

instance Graphics.Rendering.Chart.ToPlot (PlotSimplePolygons p) where
  toPlot p = Graphics.Rendering.Chart.Plot
    { _plot_render = renderSimplePolygons p
    , _plot_legend = [] -- BUG missing
    , _plot_all_points = plotAllTriangles p
    }

-- | TODO consider providing lines around each polygon.

renderSimplePolygons :: () => PlotSimplePolygons p x y -> Graphics.Rendering.Chart.PointMapFn x y -> Graphics.Rendering.Chart.BackendProgram ()
-- Don't do anything, if there are no triangles
renderSimplePolygons p pmap = mapM_ go (_plot_polygons_polygons p)
  where
    go xyp = withFillStyle (_plot_polygons_fillFun p xyp) $ do
      ps <- Graphics.Rendering.Chart.alignFillPoints $ [ mapXY pmap (x,y) | (x,y,_) <- xyp ]
      fillPointPath ps

plotAllTriangles :: PlotSimplePolygons p x y -> ([x],[y])
plotAllTriangles p = ( [ x | xyps <- _plot_polygons_polygons p, (x,_,_) <- xyps ]
                     , [ y | xyps <- _plot_polygons_polygons p, (_,y,_) <- xyps ]
                     )

genTriangles :: [(Double,Double,p)] -> [[(Double,Double,p)]]
genTriangles xyp = V.toList $ V.map (V.toList . V.map (\(Point2 x y :+ p) -> (fromRational x,fromRational y,p))) faces
  where
    triangulation = DC.delaunayTriangulation $ NE.fromList [ Point2 (toRational x) (toRational y) :+ p | (x,y,p) <- xyp ]
    faces = triangularFaces triangulation

