-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Polar
-- Copyright   :  (c) Tim Docker 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Plot data using polar @(r, theta)@ coordinates. This should probably be
-- in the @Layout@ or @Axis@ section, but for simplicity it is placed here.
--
-- At present it is unclear how many types used by the standard
-- Cartesian plots - e.g. `PlotPoints` or `ToPlot` - can be shared
-- with this. I personally am more interested in seeing how to
-- expand Chart to more-complex projections such as those used in
-- Astronomy (<http://www.atnf.csiro.au/people/mcalabre/WCS/>),
-- but there is plenty of complication if we just stick to the
-- Earth (<http://www.progonos.com/furuti/MapProj/Normal/TOC/cartTOC.html>).
--
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

-- The following is based on the Pie plot, but I would really want the
-- axis code separated out to support more generic non-Cartesian
-- projections, which likely means some form of a 2D axis type class.

module Graphics.Rendering.Chart.Plot.Polar(
  
  -- * Example
  --
  -- $example
  
  -- * Notes
  --
  -- $notes
  
  -- * Symbols
  
  PolarLayout(..)
  , PolarChart(..)
  , PolarPoints(..)
  , PolarCoord
    
  -- * Rendering  
    
  , polarToRenderable
    -- , polarChartToRenderable
    
  -- * Lenses
    
  , polarlayout_background
  , polarlayout_plot_background
  , polarlayout_title
  , polarlayout_title_style
  , polarlayout_plot
  , polarlayout_margin
    
  , polar_points
    
  , polar_points_legend
  , polar_points_style
  , polar_points_values
  )
       where

import Control.Monad (forM_)
import Control.Lens

import Data.Colour
import Data.Colour.Names

import Data.Default.Class

import Data.Char (chr)

import Graphics.Rendering.Chart.Axis.Internal (scaleLinear)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Grid (aboveN, gridToRenderable, tval, weights)
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Utils (maximum0)

-- | A polar plot of @(r,theta)@ values.
--    
--   /TODO:/
--
--    - Calculate the spacing for the theta labels rather than hard coding them
-- 
data PolarLayout = PolarLayout 
  { _polarlayout_background      :: FillStyle
    -- ^ How to fill the rectangle.
    
  , _polarlayout_plot_background :: Maybe FillStyle
    -- ^ How to fill the background of the plot, 
    --   if different from the overall background set by
    --   @_polarlayout_background@.

  , _polarlayout_title           :: String
    -- ^ Title to display above the chart.
  
  , _polarlayout_title_style     :: FontStyle
    -- ^ Font style to use for the title.

  , _polarlayout_plot            :: PolarChart
    -- ^ The data set to plot.

  , _polarlayout_margin          :: Double
    -- ^ The margin distance to use.
  }

-- | The data and axes to plot (at present only the data is configureable).
--
--   At present a set of labels indicating the radial values
--   are drawn at an angle of 45 degrees, to match the style
--   used by matplotlib - e.g. <http://matplotlib.org/gallery.html#pie_and_polar_charts> -
--   but it could be made configurable.
--
--   /TODO:/
--
--    - Add axis configuration
--
data PolarChart = PolarChart
                  { _polar_points :: [PolarPoints]
                  }

-- | Radius and angle (in radians, measured anti-/counter- clockwise from the
--   X axis). Points with a negative radius are excluded from the plot.
type PolarCoord = (Double, Double)

-- | The points to draw.
--
--   /TODO:/
--
--    - Change to something like @PolarPoints r t@
--
data PolarPoints = PolarPoints
                   { _polar_points_legend :: String
                     -- ^ The label for the points (currently unused)
                   , _polar_points_style :: PointStyle
                     -- ^ The style used to draw the points
                   , _polar_points_values :: [PolarCoord]
                     -- ^ The @(r,theta)@ values.
                   }

instance Default PolarChart where
  def = PolarChart 
    { _polar_points = []
    }
  
instance Default PolarPoints where
  def = PolarPoints
    { _polar_points_legend = ""
    , _polar_points_style  = def
    , _polar_points_values = []
    }

instance Default PolarLayout where
  def = PolarLayout 
        { _polarlayout_background = solidFillStyle $ opaque white
        , _polarlayout_plot_background = Nothing
        , _polarlayout_title           = ""
        , _polarlayout_title_style     = def { _font_size   = 15
                                             , _font_weight = FontWeightBold
                                             } 
        , _polarlayout_plot            = def
        , _polarlayout_margin          = 0
        }

instance ToRenderable PolarLayout where
  toRenderable = setPickFn nullPickFn . polarToRenderable

{- PieChart has this; not sure wanted here, so leaving out
instance ToRenderable PolarChart where
  toRenderable = setPickFn nullPickFn . (polarChartToRenderable Nothing)
-}
  
-- Hmm, may remove/rename/change
polarChartToRenderable :: 
  Maybe FillStyle
  -> PolarChart
  -> Renderable (PickFn a)
polarChartToRenderable mf pc =
  Renderable { minsize = minsizePolar pc
             , render  = renderPolar mf pc
             }
  
           
-- TODO: the rendering needs a lot of clean up

polarPlotToRenderable ::
  PolarLayout
  -> Renderable (PickFn a)
polarPlotToRenderable pl =
  let chart = _polarlayout_plot pl
      dataLayer = polarChartToRenderable (_polarlayout_plot_background pl) chart
          
  in dataLayer
     
-- | Render the data, axes, and labels.
--
--   At present the axes are always drawn first, and then the data.
--
polarToRenderable ::
  PolarLayout
  -> Renderable (PickFn a)
polarToRenderable pl =
  fillBackground (_polarlayout_background pl) $ 
  gridToRenderable grid
  where
    title = label (_polarlayout_title_style pl) HTA_Centre VTA_Top (_polarlayout_title pl)
    lm    = _polarlayout_margin pl
    
    grid = aboveN
         [ tval $ addMargins (lm/2,0,0,0) (setPickFn nullPickFn title)
         , weights (1,1) $ tval $
           addMargins (lm,lm,lm,lm) $
           polarPlotToRenderable pl
           -- for when we support legends
           -- , tval $ renderLegend pl (getLegendItems pl)
         ]

-- TODO: have to work out extra size here rather than hard code it!
extraSpace ::
  PolarChart
  -> ChartBackend (Double, Double)
extraSpace _ = return (10,10)

minsizePolar ::
  PolarChart
  -> ChartBackend (Double, Double)
minsizePolar pc = do
  (extraw, extrah) <- extraSpace pc
  return (extraw * 2, extrah * 2)

-- degrees to radians
d2r :: Double -> Double
d2r = ((pi/180.0) *) 

getMaxRadii :: 
  [PolarPoints] 
  -> [Double]
  -- ^ The maximum radii of all the points; this is the radius of the
  --   point plus the radius of the symbol.
getMaxRadii = concatMap getRs
  where
    getRs :: PolarPoints -> [Double]
    getRs pp = map getR $ _polar_points_values pp
      where
        rad = _point_radius $ _polar_points_style pp -- use Lens!
        getR = (rad +) . snd 

-- Remove invalid points (at present that means negative radii).
--
-- TODO: clean up to remove empty PolarPoints sections
-- 
filterPoints :: [PolarPoints] -> [PolarPoints]
filterPoints = map noNeg
  where
    noNeg pp = pp { _polar_points_values = filter ((>=0) . fst) (_polar_points_values pp) }

renderPolar :: 
  Maybe FillStyle
  -> PolarChart 
  -> (Double, Double) 
  -> ChartBackend (PickFn a)
renderPolar mf pc (w,h) = do
  (extraw, extrah) <- extraSpace pc
  let -- for now assume (0,0) to (w,h)
      center_x = w/2
      center_y = h/2
      -- center = Point center_x center_y
      
      -- angle at which to draw the radial axis labels
      radialAngle = d2r $ 45.0/2
      
      radius = min (w - 2*extraw) (h - 2*extrah) / 2
      
      allPoints = filterPoints $ _polar_points pc
      rMaxVals = getMaxRadii allPoints
      
      -- scale data using the max of (r+size);
      -- actually, this is now scaled by rMax after
      -- increasing to a "nice" value
      rMaxTmp = maximum0 rMaxVals
      rMax = case tickvs of
        [] -> rMaxTmp
        _ -> maximum tickvs
            
      -- where should the radial axis marks be?
      -- for now use a linear scale
      -- TODO: note that if no data, rmin,rmax will be -1, 1 which isn't wanted here.
      --       but hopefully the filtering > 0 will "fix" this (and also remove 0
      --       values which can cause problems too)
      f0 = filter (>0)
      g (xs, ys, zs) = (f0 xs, f0 ys, f0 zs)
      (tickvs, labelvs, gridvs) = g $ scaleLinear (5, 5) (0, rMaxTmp) rMaxVals
  
      -- scale the user coodinate to the screen coordinates
      scaleR r = r * radius /rMax
      coordConv :: PolarCoord -> (Double,Double)
      coordConv (r,theta) = 
        let rr = scaleR r
        in ( center_x + rr * cos theta
           , center_y - rr * sin theta ) 
      
      paints :: PolarPoints -> ChartBackend ()
      paints ps = mapM_ (paint (_polar_points_style ps)) $ _polar_points_values ps
      
      paint :: PointStyle -> PolarCoord -> ChartBackend ()
      paint pstyle rt = 
        let (x,y) = coordConv rt
            ps = pstyle { _point_radius = scaleR (_point_radius pstyle) }
        in drawPoint ps (Point x y)
        
  case mf of
    Nothing -> return ()
    Just fs -> withFillStyle fs $ do
      p <- alignFillPath $ arc' center_x center_y radius 0 (2*pi)
      fillPath p
  
  -- axes; repeated code needs to be refactored
  -- renderRadialGrid coordConv (0,rMax) gridvs
  renderRadialGrid coordConv (0,rMax) gridvs
  renderRadialAxis coordConv (0,rMax) labelvs radialAngle
  
  -- data
  forM_ allPoints paints
    
  return nullPickFn
      
-- don't do r<=min and r>=max
renderRadialGrid ::
  (PolarCoord -> (Double,Double))
  -- ^ convert from r,theta to plot coordinates
  -> (Double, Double)
  -- ^ (min,max) values for the radial axis
  -> [Double]          -- ^ grid positions
  -> ChartBackend ()
renderRadialGrid conv (rmin,rmax) gridvs = do
  let (cx, cy) = conv (0,0)
      p0 = Point cx cy
      step = 45
      angles = map d2r [0, step .. (360-step)]
      
      fgridvs = filter (\r -> r > rmin && r < rmax) gridvs 
      
  -- having to scale the radius via conv is a bit pointless
  withLineStyle (dashedLine 1 [2,4] (opaque lightgrey)) $ do
    -- constant r
    forM_ fgridvs $ \r -> 
      let (cx2, _) = conv (r,0)
      in strokePath $ arc' cx cy (cx2-cx) 0 (2*pi)
    
    -- constant theta
    forM_ angles $ \theta ->
      let (cx2, cy2) = conv (rmax,theta)
      in alignStrokePoints [p0, Point cx2 cy2] >>= strokePointPath
         
-- Does both r and theta
renderRadialAxis ::
  (PolarCoord -> (Double,Double))
  -- ^ convert from r,theta to plot coordinates
  -> (Double, Double)
  -- ^ (min,max) values for the radial axis
  -> [Double]
  -- ^ label values and positions
  -> Double
  -- ^ angle (degrees, anti/counter clockwise) for the radial labels to be drawn
  -> ChartBackend ()
renderRadialAxis conv (_,rmax) labelvs rAngle = do
  let ls = solidLine 1 $ opaque black
      -- ag = 10
      
      lblstyle = def
      
      -- theta = 0 -- could be 45/...
      -- axisPoint r = uncurry Point $ conv r theta
      
      -- start = axisPoint rmin
      -- end   = axisPoint rmax
  
      -- here assuming r=0 is the center
      (cx,cy) = conv (0,0)
      (cx2,_) = conv (rmax,0)
      cr = cx2 - cx
      
      -- this only works when theta=0
      -- vconv = Vector 0
      -- tp = vconv 1
      
      {-
      drawTick (value,len) =
        let t1 = axisPoint value
            t2 = t1 `pvadd` (vscale len tp)
        in alignStrokePoints [t1,t2] >>= strokePointPath 
      -}
      
      -- this only works when theta=0
      -- TODO: do we need to bother about label overlaps here?
      {-
      drawLabels (offset,lbs) = do
        labels' <- avoidOverlaps lbs
        mapM_ drawLabel labels'
        where
          drawLabel (value,s) = do
            drawTextA hta vta (axisPoint value `pvadd` vconv offset) s
            textDimension s  
           
      avoidOverlaps lbs = do
        rects <- mapM labelDrawRect lbs
        return $ map snd . head . filter (noOverlaps . map fst)
               $ map (`eachNth` rects) [0 .. length rects] 
  
      labelDrawRect (value,s) = do
        let pt = axisPoint value `pvadd` vconv ag
        r <- textDrawRect hta vta pt s
        return (hBufferRect r,(value,s))
      -}      
      
      -- tickPos = [0, 1 .. floor rmax]
      
      -- using toEnum to convert Int to Double is a bit obscure
      -- ticks = zipWith (\x o -> (toEnum x, o + if x `mod` 5 == 0 then 2 else 0)) tickPos $ repeat 4
      
      -- have a single offset for now
      {-
      majorTicks = filter (\x -> x `mod` 5 == 0) tickPos
      labels = [zipWith (\x l -> (toEnum x, l)) majorTicks (map show majorTicks)]
      -}  
      
  -- radial axis
  {-
  withLineStyle (ls {_line_cap = LineCapSquare}) $ do
    p <- alignStrokePoints [start, end]
    strokePointPath p
  
  withLineStyle (ls {_line_cap = LineCapButt}) $ do
    mapM_ drawTick ticks
  -}    
      
  -- theta axis
  withLineStyle ls $
    strokePath $ arc' cx cy cr 0 (2*pi)
  
  -- axis labels
  -- TODO: improve positioning of labels
  withFontStyle lblstyle $ do
    -- radial
    {-
    labelSizes <- mapM (mapM textDimension) (labelTexts labels)
    let sizes = map ((+ag).maximum0.map snd) labelSizes
        offsets = scanl (+) ag sizes
    mapM_ drawLabels (zip offsets labels)
    -}
    
    forM_ labelvs $ \r -> do
      let (lx,ly) = conv (r,rAngle)
          txt = show r
      -- choice of offset depends on rAngle; current choice is okay for 22.5 degrees
      drawTextA HTA_Left VTA_Bottom (Point lx ly) txt
    
    -- theta (no checks like there are with the radial labels)
    let tlabels = [0, 45 .. 315::Int]
    forM_ tlabels $ \t -> do
      -- QUS: how best to do the offset?
      let (lx,ly) = conv (rmax*1.05, d2r (fromIntegral t))
          txt = show t ++ [chr 176] -- add in the degree symbol
      drawTextA HTA_Centre VTA_Centre (Point lx ly) txt


-- Set up the lenses

$( makeLenses ''PolarLayout )
$( makeLenses ''PolarChart )
$( makeLenses ''PolarPoints )

-- Documentation
--
-- $example
--
-- /WARNING/
--
-- The image shown here is currently out of date:
--
--   - a bug in the code means that the axis auto-scaling is not working, so
--     that the maxium radius is 7, and the radial grid lines do not match up
--     with the axis labels.
--
-- This is based on the scatter-plot version from matplotlib,
-- <http://matplotlib.org/examples/pie_and_polar_charts/polar_scatter_demo.html>.
--
-- <<docimages/polar-example1.svg>>
--
-- Note that, at present, the Diagrams SVG back end is not guaranteed to
-- dispay the filled circles. Hopefully it will be fixed before this code
-- has been fixed up; see <https://github.com/timbod7/haskell-chart/issues/19>
-- for more information.
--
-- > import qualified Graphics.Rendering.Chart.Backend.Cairo as C
-- >
-- > import Control.Lens
-- > import Control.Monad (replicateM)
-- > import Data.Colour
-- > import Data.Colour.Names
-- > import Data.Default.Class
-- > import Data.List (zip4)
-- > import Graphics.Rendering.Chart
-- > import Graphics.Rendering.Chart.Utils (LUT, fromLUT, cubeHelix0)
-- > import System.Random
-- >
-- > -- randomly chose r and theta values; the circle radius scales with
-- > -- r, and the color maps to theta via the LUT.
-- > makeData :: LUT (Colour Double) -> IO [PolarPoints]
-- > makeData lut = do
-- >   let rand = replicateM 150 (randomIO :: IO Double)
-- >   r1 <- rand
-- >   r2 <- rand
-- >   let rs = map (2*) r1
-- >       rads = map (*0.1) rs
-- >       cols = map (flip withOpacity 0.4 . fromLUT lut) r2
-- >       thetas = fmap (2*pi*) r2
-- >       pitem (r,t,s,c) = polar_points_style .~ pstyle s c
-- >                       $ polar_points_values .~ [(r,t)]
-- >                       $ def
-- >       pstyle s c = point_color .~ c
-- >                    $ point_radius .~ s
-- >                    $ point_border_color .~ opaque black
-- >                    $ point_border_width .~ 1
-- >                    $ def
-- >                    
-- >   return $ map pitem $ zip4 rs thetas rads cols
-- > 
-- > bgFill, pFill :: FillStyle
-- > bgFill = solidFillStyle (opaque gray)
-- > pFill = solidFillStyle (withOpacity orange 0.4)
-- >       
-- > testPlot :: [PolarPoints] -> PolarLayout
-- > testPlot pps = 
-- >   polarlayout_title .~ "Polar plot"
-- >     $ polarlayout_background .~ bgFill
-- >     $ polarlayout_plot_background .~ Just pFill
-- >     $ polarlayout_plot . polar_points .~ pps
-- >     $ polarlayout_margin .~ 10
-- >     $ def
-- > 
-- > makePlot :: IO ()
-- > makePlot = do
-- >   data <- makeData cubeHelix0
-- >   let r = toRenderable (testPlot data)
-- >   _ <- C.renderableToFile (C.FileOptions (600,600) C.SVG) r "polar.svg"
--

-- $notes
--
-- This is /not/ intended as a usable API, more as an exploration of the
-- design space. Issues include:
--
--   - support for lines, histogram-style plots, ...
--
--   - allow theta values to be given in radians or degrees
--
--   - how to combine multiple data sets
--
--   - fix up axis positioning and size calculation
--
--   - add configuration to allow for adjusting the angle at which the
--     radius is displayed, the position angle of 0, switch to clockwise
--     direction for theta, label in radians, ...
--
--   - generalize the approach for other non-cartesian projections
--
--   - add support for legends
--
