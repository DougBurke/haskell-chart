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
{-# LANGUAGE ScopedTypeVariables #-} -- only needed to hide a defaulting warning

-- The following is based on the Pie plot, but I would really want the
-- axis code separated out to support more generic non-Cartesian
-- projections, which likely means some form of a 2D axis type class.

-- theta is &#x03b8;
-- pi    is &#x03c0;

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
  , polar_radial_axis_offset
  , polar_theta_axis_degrees
  , polar_theta_axis_zero
  , polar_theta_axis_nticks
  , polar_theta_axis_reverse
    
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

import Graphics.Rendering.Chart.Axis.Internal (scaleLinear, showD)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Grid (aboveN, gridToRenderable, tval, weights)
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Utils (maximum0)

-- | A polar plot of @(r,&#x03b8;)@ values.
--    
--   /TODO:/
--
--    - Calculate the spacing for the &#x03b8; labels rather than hard coding them
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
--   /TODO:/
--
--    - Add axis configuration/move into relevant data structures
--
data PolarChart = PolarChart
                  { _polar_points :: [PolarPoints]
                  , _polar_radial_axis_offset :: Double
                    -- ^ The offset, in radians, from the location of the
                    --   @&#x03b8;=0@ axis at which to label the radial axis.
                    --   The default is @&#x03c0;/8@.
                  , _polar_theta_axis_degrees :: Bool
                    -- ^ Label the &#x03b8; axis in degrees (@True@) or radians (@False@),
                    --   in the latter case using \"units\" of @&#x03c0;@ (e.g. @&#x03c0;/2@).
                    --   The default is @True@.
                  , _polar_theta_axis_zero :: Double
                    -- ^ Angle, in radians, measured counter-/anti- clockwise from the horizontal,
                    --   to use for @&#x03b8;=0@. The default is @0@.
                  , _polar_theta_axis_nticks :: Int
                    -- ^ The number of tick marks on the theta axis; the default is 8 which gives
                    --   a spacing of 45 degrees. The value is assumed to be greater than 1.
                  , _polar_theta_axis_reverse :: Bool
                    -- ^ Set to @True@ to have the angles be measured clockwise; the
                    --   default is @False@.
                  }

-- | Radius and &#x03b8; (in radians, measured anti-/counter- clockwise from the
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
                     -- ^ The @(r,&#x03b8;)@ values.
                   }

instance Default PolarChart where
  def = PolarChart 
    { _polar_points = []
    , _polar_radial_axis_offset = d2r (45.0/2)
    , _polar_theta_axis_degrees = True
    , _polar_theta_axis_zero = 0
    , _polar_theta_axis_nticks = 8
    , _polar_theta_axis_reverse = False
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
        rad = _point_radius $ _polar_points_style pp
        getR = (rad +) . fst

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
      
      radialAngle = _polar_radial_axis_offset pc
      nThetaTicks = _polar_theta_axis_nticks pc
      
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

      -- scale from the user theta value (in radians) to the on-screen value
      getTheta theta = _polar_theta_axis_zero pc +
                       if _polar_theta_axis_reverse pc then (-theta) else theta
  
      -- scale the user coodinate to the screen coordinates
      scaleR r = r * radius /rMax
      coordConv :: PolarCoord -> (Double,Double)
      coordConv (r,theta) = 
        let rr = scaleR r
            theta' = getTheta theta
        in ( center_x + rr * cos theta'
           , center_y - rr * sin theta' ) 
      
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
  renderRadialGrid coordConv (0,rMax) nThetaTicks gridvs
  renderRadialAxis coordConv (0,rMax) nThetaTicks labelvs radialAngle (_polar_theta_axis_degrees pc)
  
  -- data
  forM_ allPoints paints
    
  return nullPickFn
      
-- don't do r<=min and r>=max
renderRadialGrid ::
  (PolarCoord -> (Double,Double))
  -- ^ convert from @(r,theta)@ to plot coordinates
  -> (Double, Double)
  -- ^ (min,max) values for the radial axis
  -> Int
  -- ^ Number of tick marks for the theta axis
  -> [Double]
  -- ^ grid positions
  -> ChartBackend ()
renderRadialGrid conv (rmin,rmax) nticks gridvs = do
  let (cx, cy) = conv (0,0)
      p0 = Point cx cy
      step = 360 / fromIntegral nticks
      angles = map d2r [0, step .. (360-step)]
      
      fgridvs = filter (\r -> r > rmin && r < rmax) gridvs 
      
  -- having to scale the radius via conv is a bit pointless
  withLineStyle (dashedLine 1 [2,4] (opaque lightgrey)) $ do
    -- constant r
    forM_ fgridvs $ \r -> 
      let (cx2, cy2) = conv (r,0)
          r' = sqrt $ (cx2-cx)*(cx2-cx) + (cy2-cy)*(cy2-cy)
      in strokePath $ arc' cx cy r' 0 (2*pi)
    
    -- constant theta
    forM_ angles $ \theta ->
      let (cx2, cy2) = conv (rmax,theta)
      in alignStrokePoints [p0, Point cx2 cy2] >>= strokePointPath

-- Represent label positions about the polar plot by what "zone" they are
-- in. The zone is based on quadrants, but I provide special cases for
-- positions close to n pi / 2 (n=0, 1, 2, 3)

data Zone =
  ZR | ZT | ZL | ZB -- the four "cardinal" locations (right, top, left, or bottom)
  |
  ZQ1 | ZQ2 | ZQ3 | ZQ4 -- the four quadrants
  deriving Eq
           
-- convert theta, in radians, measured anti-clockwise from the horizontal,
-- into a zone, ignoring the loss of precision due to rounding/using a
-- 'normalized' angle
getZone :: Double -> Zone
getZone t =
  let (_::Integer,f) = properFraction $ t / (2*pi)
      tnorm = 4 * if t < 0 then 1 + f else f
      (z,_) = properFraction tnorm

      -- try a tolerance of ~5 degrees
      isNear a b = abs (a-b) < 0.05
      
      locs = [(0, ZR), (1, ZT), (2, ZL), (3, ZB), (4, ZR)]
      zones = [ZQ1, ZQ2, ZQ3, ZQ4, ZQ1]
      ls = filter (isNear tnorm . fst) locs
      
  in case ls of
    ((_,zv):_) -> zv
    _          -> zones !! z
             
-- TODO: calculate dx,dy offset for point, assuming input point is at
--       the edge of the plot
--
-- TODO: vertical spacing should be based on center or baseline?
thetaLabelOffsets ::
  Double
  -- ^ angle at which to draw the label (radians, measured counter-clockwise
  --   from the horizontal axis)
  -> (HTextAnchor, VTextAnchor)
thetaLabelOffsets t = case getZone t of
  ZR  -> (HTA_Left,   VTA_Centre)
  ZL  -> (HTA_Right,  VTA_Centre)
  ZT  -> (HTA_Centre, VTA_Bottom)
  ZB  -> (HTA_Centre, VTA_Top)
  ZQ1 -> (HTA_Left,   VTA_Bottom)
  ZQ2 -> (HTA_Right,  VTA_Bottom)
  ZQ3 -> (HTA_Right,  VTA_Top)
  ZQ4 -> (HTA_Left,   VTA_Top)
          
-- Draw the label at the given location, using offsets         
-- that are hopefully appropriate (using the actual angle
-- of the line with respect to the center, rather than the
-- coordinate angle, since this may be reversed and/or
-- offset).
--
drawTextTheta ::
  (PolarCoord -> (Double, Double))
  -- ^ convert from r,theta to plot coordinates
  -> PolarCoord
  -> String
  -> ChartBackend ()
drawTextTheta conv p txt =
  let (lx,ly) = conv p
      (cx,cy) = conv (0,0)
      dx = lx - cx
      dy = cy - ly -- note: y increases down
      (lxoff,lyoff) = thetaLabelOffsets $ atan2 dy dx
  in drawTextA lxoff lyoff (Point lx ly) txt
                        
piChar :: Char
piChar = chr 0x03c0

piFrac :: Int -> Int -> String
piFrac 1 1 = [piChar]
piFrac 1 n = piChar : "/" ++ show n
piFrac j 1 = show j ++ [piChar]
piFrac j n = show j ++ piChar : "/" ++ show n

thetaLabel :: 
  Int -- ^ number of ticks, assumed to be > 1
  -> Int -- ^ current tick number (0 to nticks-1)
  -> String -- ^ label (using the pi character)
thetaLabel _ 0 = "0"
thetaLabel n i = 
  let j = 2*i
      g = gcd j n
  in if g == 1
     then piFrac j n
     else piFrac (j `div` g) (n `div` g)

-- Does both r and theta
renderRadialAxis ::
  (PolarCoord -> (Double,Double))
  -- ^ convert from r,theta to plot coordinates
  -> (Double, Double)
  -- ^ (min,max) values for the radial axis
  -> Int
  -- ^ Number of tick marks for the theta axis
  -> [Double]
  -- ^ label values and positions
  -> Double
  -- ^ angle (degrees, anti/counter clockwise) for the radial labels to be drawn
  -> Bool
  -- ^ @True@ if label using degrees, otherwise radians
  -> ChartBackend ()
renderRadialAxis conv (_,rmax) nticks labelvs rAngle useDeg = do
  let ls = solidLine 1 $ opaque black
      lblstyle = def
      
      -- here assuming r=0 is the center; would be a bit simpler to
      -- get cr if also knew the theta offset
      (cx,cy) = conv (0,0)
      (cx2,cy2) = conv (rmax,0)
      cr = sqrt $ (cx2-cx)*(cx2-cx) + (cy2-cy)*(cy2-cy)
      
  -- theta axis
  withLineStyle ls $
    strokePath $ arc' cx cy cr 0 (2*pi)
  
  -- axis labels
  -- TODO: improve positioning of labels
  withFontStyle lblstyle $ do
    -- radial
    --
    -- TODO: using thetaLabelOffsets isn't quite right here - e.g.
    -- if the axis were horizontal we'd want HTA_Center VTA_Top
    -- rather than HTA_Left VTA_Center but leave as is for now
    forM_ labelvs $ \r -> drawTextTheta conv (r,rAngle) $ show r
    
    -- theta
    let tindex = [0 .. nticks-1]
        tstep = 360 / fromIntegral nticks
        toffset = 0 :: Double

    forM_ tindex $ \ti -> 
      let theta = fromIntegral ti * tstep + toffset
          thetar = d2r theta
          txt = if useDeg
                then showD theta ++ [chr 176] -- add in the degree symbol
                else thetaLabel nticks ti 
          
      in drawTextTheta conv (rmax*1.05,thetar) txt

-- Set up the lenses

$( makeLenses ''PolarLayout )
$( makeLenses ''PolarChart )
$( makeLenses ''PolarPoints )

-- Documentation
--
-- $example
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
-- > import Data.List (zip5)
-- > import Graphics.Rendering.Chart
-- > import Graphics.Rendering.Chart.Utils (LUT, fromLUT, cubeHelix0)
-- > import System.Random
-- >
-- > -- randomly chose r and theta values; the circle radius scales with
-- > -- r, and the color maps to theta via the LUT.
-- > makeData :: LUT (Colour Double) -> IO [PolarPoints]
-- > makeData lut = do
-- >   let npts = 150
-- >       rand = replicateM npts (randomIO :: IO Double)
-- >       shapes = [ PointShapeCircle
-- >                , PointShapePolygon 6 True
-- >                , PointShapePlus
-- >                , PointShapeCross
-- >                , PointShapeStar
-- >                ]
-- >   r1 <- rand
-- >   r2 <- rand
-- >   shps <- replicateM npts (randomRIO (0,length shapes -1))
-- >   let rs = map (2*) r1
-- >       rads = map (*0.1) rs
-- >       cols = map (flip withOpacity 0.4 . fromLUT lut) r2
-- >       thetas = fmap (2*pi*) r2
-- >       pitem (r,t,s,c,shp) = polar_points_style .~ pstyle s c shp
-- >                           $ polar_points_values .~ [(r,t)]
-- >                           $ def
-- >       pstyle s c shp = point_shape .! shapes !! shp
-- >                      $ point_color .~ c
-- >                      $ point_radius .~ s
-- >                      $ point_border_color .~ opaque black
-- >                      $ point_border_width .~ 1
-- >                      $ def
-- >                    
-- >   return $ map pitem $ zip5 rs thetas rads cols
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
-- >   r <- toRenderable . testPlot <$> makeData cubeHelix0
-- >   _ <- C.renderableToFile (C.FileOptions (400,400) C.SVG) r "polar.svg"
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
