{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-} -- only needed to hide a defaulting warning
{-# LANGUAGE MultiParamTypeClasses #-} -- for PolarPlotValue

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Polar
-- Copyright   :  (c) Tim Docker 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Plot data using polar @(r, &#x03b8;)@ coordinates. This should probably be
-- in the @Layout@ or @Axis@ section, but for simplicity it is placed here
-- (some refactoring is currently taking place to make better use of the
-- existing code/functionality).
--
-- I personally am more interested in seeing how to
-- expand Chart to more-complex projections such as those used in
-- Astronomy (<http://www.atnf.csiro.au/people/mcalabre/WCS/>),
-- but there is plenty of complication if we just stick to the
-- Earth (<http://www.progonos.com/furuti/MapProj/Normal/TOC/cartTOC.html>).
--
-----------------------------------------------------------------------------

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
  , PolarLayoutAxes(..)
  , PolarAxesData(..)
  , PolarPlotValue(..)    
  , Radians(..)
  , Degrees(..)
  , ScaleRadialAxis
  , ScaleThetaAxis
    
  -- * Rendering  
    
  , polarLayoutToRenderable
    
  -- * Utility routines for creating and displaying axes
  --    
  -- | These may be removed.  
  , autoScaledAxes
  , showThetaLabelDegrees
  , showThetaLabelRadians
    
  -- * Lenses
    
  , polarlayout_background
  , polarlayout_plot_background
  , polarlayout_title
  , polarlayout_title_style
  , polarlayout_axes
  , polarlayout_plots
  , polarlayout_legend
  , polarlayout_margin
    
  -- , polar_radial_axis_offset
  -- , polar_theta_axis_nticks
  -- , polar_theta_axis_margin
  -- , polar_theta_axis_reverse
  , polar_grid_style
  , polar_theta_axis_style
  , polar_radial_axis_style
  , polar_axes_label_style
  , polar_axes_filter_plots    
  , polar_axes_generate
    
  , polaraxes_viewport
  , polaraxes_r_range
  , polaraxes_theta_zero
  , polaraxes_r_visibility
  , polaraxes_r_ticks
  , polaraxes_r_labels
  , polaraxes_r_grid
  , polaraxes_t_visibility
  , polaraxes_t_ticks
  , polaraxes_t_labels
  , polaraxes_t_grid
  )
       where

import Control.Arrow ((***))
import Control.Monad (forM_)
import Control.Lens

import Data.Colour
import Data.Colour.Names

import Data.Default.Class

import Data.Char (chr)

import Graphics.Rendering.Chart.Axis.Internal (scaleLinear, showD)
-- import Graphics.Rendering.Chart.Axis (AxisStyle(..), AxisVisibility(..), PlotValue(..))
import Graphics.Rendering.Chart.Axis (AxisVisibility(..), PlotValue(..), autoScaledAxis)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Grid (aboveN, besideN, gridToRenderable, tval, weights)
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Utils (maximum0)

{-
NOTES:

In the middle of converting the code to an approach closer to the cartesian case
used by Layout/Axis/Plot. It is not immediately clear how close a match we can
get.

At present there is no analog of the AxisT record, and much of the information
about tick marks and grid location is hard coded (this is a loss compared
to the code prior to this rework).

-}

-- | A polar plot of @(r,&#x03b8;)@ values.
--    
--   /TODO:/
--
--    - A lot of the customization present in earlier versions has
--      been removed (or lost) in this version.
-- 
data PolarLayout r t = PolarLayout 
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

  , _polarlayout_axes            :: PolarLayoutAxes r t
    -- ^ Axes for the layout.
    
  , _polarlayout_plots           :: [Plot r t]
    -- ^ The data to plot.

  , _polarlayout_legend          :: Maybe LegendStyle
    -- ^ Style the legend area
    
  , _polarlayout_margin          :: Double
    -- ^ The margin distance to use.
  }

-- | Configuration of the axes of the polar plot.
--
--   /TODO:/
--
--    - this is in a state of flux and is liable to change
--
data PolarLayoutAxes r t = 
  PolarLayoutAxes 
{-  
  { _polar_radial_axis_offset :: t
    -- ^ The offset from the location of the
    --   @&#x03b8;=0@ axis at which to label the radial axis.
    --
    --   CURRENTLY UNUSED.
  , _polar_theta_axis_nticks :: Int
    -- ^ The number of tick marks on the theta axis; the default is 8 which gives
    --   a spacing of 45 degrees. The value is assumed to be greater than 1.
  , _polar_theta_axis_margin :: Double
    -- ^ The distance between the maximum radius and the theta labels,
    --   in display coordinates
  , _polar_theta_axis_reverse :: Bool
    -- ^ Set to @True@ to have the angles be measured clockwise; the
    --   default is @False@.
  , 
-}
  { _polar_grid_style :: LineStyle
    -- ^ The color and style used to draw the grid lines. The default is
    --   a dashed, light gray, line.
  , _polar_theta_axis_style :: LineStyle
    -- ^ The color and style used to draw the theta axis (that is, the
    --   circumference of the plot). The default is a solid black line.
  , _polar_radial_axis_style :: LineStyle
    -- ^ The color and style used to draw the radial axis.
    --   The default is for there to be no line.
  , _polar_axes_label_style :: FontStyle
    -- ^ The font used to draw the axis labels (both radius and @&#x03b8;@).
  
  , _polar_axes_filter_plots :: [Plot r t] -> [Plot r t]
    -- ^ Filter out data points and, if the resulting plot is empty,
    --   plots.
    
  , _polar_axes_generate :: ([r],[t]) -> PolarAxesData r t
    -- The Layout/Axis version has _generate be :: AxisFn x which is [x] -> AxisData x
    -- and AxisT x = AxisT RectEdge AxisStyle Bool (AxisData x)
    -- and AxisData contains mapping/ticks/grid info
  }

instance (PolarPlotValue r t, Num r, Ord r) => Default (PolarLayoutAxes r t) where
  def = PolarLayoutAxes
  {-
    { _polar_radial_axis_offset = d2r (45.0/2)
    , _polar_theta_axis_nticks = 8
    , _polar_theta_axis_margin = 10
    , _polar_theta_axis_reverse = False
  -}
    { _polar_grid_style = dashedLine 1 [2,4] $ opaque lightgrey
    , _polar_theta_axis_style = solidLine 1 $ opaque black
    , _polar_radial_axis_style = solidLine 0 transparent 
    , _polar_axes_label_style = def
                                
    , _polar_axes_filter_plots = filterPlots                                
    , _polar_axes_generate = autoPolarAxes
    }
  
instance (PolarPlotValue r t, Num r, Ord r) => Default (PolarLayout r t) where
  def = PolarLayout 
        { _polarlayout_background = solidFillStyle $ opaque white
        , _polarlayout_plot_background = Nothing
        , _polarlayout_title           = ""
        , _polarlayout_title_style     = def { _font_size   = 15
                                             , _font_weight = FontWeightBold
                                             }
        , _polarlayout_axes            = def
        , _polarlayout_plots           = []
        , _polarlayout_legend          = Nothing
        , _polarlayout_margin          = 0
        }

-- | What types can be plotted as a pair of polar (r,theta) 
--   coordinates.
--  
class (PlotValue r, PlotValue t) => (PolarPlotValue r t) where
  autoPolarAxes :: ([r],[t]) -> PolarAxesData r t

{-
-- The following requires FlexibleInstances and (I would assume,
-- OverlappingInstances), so manually create the instances
instance (RealFloat r, RealFloat t) => PolarPlotValue r t where
  autoPolarAxes = autoScaledAxes scaleLinear scaleTheta 0 False
-}
  
-- | The theta values are in radians, measured anti-/counter-  
--   clockwise from the horizontal. The angles are labelled 
--   in degrees.  
instance PolarPlotValue Double Double where
  autoPolarAxes = autoScaledAxes scaleLinear 
                  (scaleTheta showThetaLabelDegrees) 0 False

-- | The angles are labelled in radians (as units of &#x03c0;).
instance PolarPlotValue Double Radians where
  autoPolarAxes = autoScaledAxes scaleLinear 
                  (scaleTheta showThetaLabelRadians) 0 False

-- | The angles are labelled in degrees.
instance PolarPlotValue Double Degrees where
  autoPolarAxes = autoScaledAxes scaleLinear 
                  (scaleTheta showThetaLabelDegrees) 0 False

type ScaleRadialAxis a =   
  (Int,Int) -> (a,a) -> [a] -> ([a], [a], [a])
  
type ScaleThetaAxis a = 
  [a] -> ([a], [(a,String)], [a])

{-
TODO: 

Can the "angle at which theta= is to be drawn" be handled by a
rotation before drawing (since this then means that it can be
specified later, rather than here). This requires a translation to the
center too.

In fact, would this be a sensible approach; i.e. translate to the
origin and make all positions relative to that?

-}

-- | Create grid marks for the theta axis. This uses labels in
--   degrees for the whole range (0 to 360) measured anti-/counter-
--   clockwise from the horizontal.
--
--   /TODO:/
--
--      - can this be made user configureable?
--
--      - should this include the margin offset; i.e. return
--        (r,t,String) for the labels
--
scaleTheta :: 
  RealFloat t
  => (Int -> Int -> String)
  -> [t] 
  -> ([t], [(t,String)], [t])
  -- ^ (ticks, labels, grids)
scaleTheta showLabel _ = 
  let nticks = 8 -- TODO: configurable?
      tindex = [0..nticks-1]
      tstep = 360 / fromIntegral nticks
      gridvs = map (\i -> d2r tstep * fromIntegral i) tindex
      labelvs = zip gridvs $ map (showLabel nticks) tindex
      tickvs = gridvs

  in (tickvs, labelvs, gridvs)

autoScaledAxes :: 
  (RealFloat r, RealFloat t)
  => ScaleRadialAxis r
  -> ScaleThetaAxis t
  -> t
  -- ^ Angle (in radians, measued counter-clockwise from the X axis)
  --   at which the theta=0 axis is to be drawn
  -> Bool 
  -- ^ Is the theta axis reversed?
  -> ([r],[t])
  -- ^ (r,theta) values
  -> PolarAxesData r t
autoScaledAxes scaleR scaleT tZero tRev (rs,ts) = 
  let rMin = 0 -- TODO: allow rmin /= 0
      
      rMaxTmp = maximum0 rs
      rMax = case rtickvs of
        [] -> rMaxTmp
        _ -> maximum rtickvs
            
      -- try and remove "problem" positions/values
      f0 = filter (>0)
      g (xs, ys, zs) = (f0 xs, f0 ys, f0 zs)
      (rtickvs, rlabelvs, rgridvs) = g $ scaleR (5, 5) (0, rMaxTmp) rs

      -- SHOULD this filtering be applied to all the outputs of scaleR?
      --
      -- we do not want grid lines at r=rmin and r=rmax
      frgridvs = filter (\r -> r > rMin && r < rMax) rgridvs

      -- scale from the user theta value (in radians) to the on-screen value
      getTheta theta = tZero + if tRev then (-theta) else theta
  
      -- can we use _polar_radial_axis_offset (or move this info
      -- around so we can use it)?
      rAngle = d2r (45.0/2)

      (ttickvs, tlabelvs, tgridvs) = scaleT ts

      coordConv (w,h) radius (r,theta) = 
        let rr = realToFrac r * radius / realToFrac rMax
            theta' = realToFrac $ getTheta theta
        in ( w/2 + rr * cos theta'
           , h/2 - rr * sin theta' ) 
      
  in PolarAxesData {
    _polaraxes_viewport = coordConv
    -- , _polaraxes_tropweiv = undefined
                            
    , _polaraxes_r_range = (rMin, rMax)
    , _polaraxes_theta_zero = tZero
    , _polaraxes_r_visibility = def
    , _polaraxes_r_ticks = rtickvs
    , _polaraxes_r_labels = zip3 rtickvs (repeat rAngle) $ map showD rlabelvs
    , _polaraxes_r_grid = frgridvs
                           
    , _polaraxes_t_visibility = def
    , _polaraxes_t_ticks = ttickvs 
    , _polaraxes_t_labels = tlabelvs
    , _polaraxes_t_grid = tgridvs
    }
    
-- | A value in radians. 
--
--   The only reason for this type (at the moment) is to
--   switch to using radians as the theta axis label.
--   This seems a bit excessive. Note that there is
--   currently no support for specialized labelling when
--   used as a `PlotValue`.
newtype Radians = Radians { unRadians :: Double }
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)

-- | The show instance just displays the numeric value.
--
instance Show Radians where
  show = show . unRadians

-- | It is not clear yet whether we will try and ensure that
--   values are kept within the range (0,2&#x03c0;].
--
--   At present the value is treated just as a `Double`;
--   that is, labelling is not in units of &#x03c0; nor
--   is the range limited to (0,2&#x03c0;].
--  
instance PlotValue Radians where
  toValue   = unRadians
  fromValue = Radians
  autoAxis  = autoScaledAxis def
    
-- | A value in degrees.
--
newtype Degrees = Degrees { unDegrees :: Double }
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)

-- | The show instance just displays the numeric value.
instance Show Degrees where
  show = show . unDegrees

-- | It is not clear yet whether we will try and ensure that
--   values are kept within the range (0,360].
--
--   At present the value is treated just as a `Double`;
--   that is, labelling does not add a degrees symbol nor
--   is the range limited to (0,360].
--  
instance PlotValue Degrees where
  toValue   = d2r . unDegrees
  fromValue = Degrees . r2d
  autoAxis  = autoScaledAxis def
    
{-

-- Axis/Types provides makeAxis but not clear what interface should
-- be here, since coord conversion needs more info  
  
-- intended to be equivalent to AxisT
--
-- TODO: note theta axis direction/offset here?
--
-- CURRENTLY UNUSED

data PolarAxesT r t = 
  PolarAxesT 
  { _polaraxest_edges :: RectEdge
  , _polaraxest_styles :: (AxisStyle,AxisStyle)
  , _polaraxest_data :: PolarAxesData r t
  }

-}
  
-- intended to be equivalent to AxisData
--
-- do viewport/tropweiv need the width/height sent in
-- or some other scaling information? or is this sent in
-- when creating this structure - e.g. in autoPolarAxes?
-- It would seem that RectSize should be known about by
-- viewport (ie can be hardcoded) rather than re-calculated
-- each time
--
  
-- | This is intended to be similar to the `AxisData`
--   type but is somewhat of a grab-bag of functionality
--   at this time, in part because there is currently no 
--   equivalent of `AxisT`.  
--  
data PolarAxesData r t =
  PolarAxesData 
  {
    -- TODO: should the viewport return Maybe for invalid (r,t) values?
    _polaraxes_viewport :: RectSize 
                           -> Double -- maximum radius, in pixel units
                           -> (r,t) -> (Double, Double)
                           
  -- , _polaraxes_tropweiv :: RectSize -> (Double, Double) -> (r,t)
    
  , _polaraxes_r_range :: (r,r)  
    -- ^ The minimum and maximum r shown.
    
  , _polaraxes_theta_zero :: t
    -- ^ Angle, in radians, measured counter-/anti- clockwise from the horizontal,
    --   to use for @&#x03b8;=0@.
    
  , _polaraxes_r_visibility :: AxisVisibility
    
  , _polaraxes_r_ticks :: [r]
  , _polaraxes_r_labels :: [(r, t, String)]
  , _polaraxes_r_grid :: [r]
    -- ^ This is assumed not to include the outer radius.
  
  , _polaraxes_t_visibility :: AxisVisibility
    
  , _polaraxes_t_ticks :: [t]
  , _polaraxes_t_labels :: [(t, String)]
  , _polaraxes_t_grid :: [t]

  }

instance ToRenderable (PolarLayout r t) where
  toRenderable = setPickFn nullPickFn . polarLayoutToRenderable

-- | Render the data, axes, and labels.
--
--   At present the axes are always drawn first, and then the data.
--
polarLayoutToRenderable ::
  PolarLayout r t
  -> Renderable (PickFn a)
polarLayoutToRenderable pl =
  fillBackground (_polarlayout_background pl) $ 
  gridToRenderable grid
  where
    lm    = _polarlayout_margin pl

    r = Renderable { minsize = minsizePolarLayout pl, render = renderPolarLayout pl }
    
    grid = aboveN
         [ tval $ titleToRenderable lm (_polarlayout_title_style pl) (_polarlayout_title pl)
         , weights (1,1) $ tval $ addMargins (lm,lm,lm,lm) r
         , tval $ renderLegends pl
         ]

titleToRenderable :: Double -> FontStyle -> String -> Renderable (PickFn a)
titleToRenderable _  _  "" = emptyRenderable
titleToRenderable lm fs s = addMargins (lm/2,0,0,0) (setPickFn nullPickFn title)
  where
    title = label fs HTA_Centre VTA_Centre s

-- Layout has getLegendItems and renderLegend which I have squashed into one for now
    
renderLegends :: PolarLayout r t -> Renderable (PickFn a)
renderLegends pl =
  let legItems = concatMap _plot_legend $ _polarlayout_plots pl
      
      g = besideN [ tval $ mkLegend (_polarlayout_legend pl) (_polarlayout_margin pl) legItems
                  , weights (1,1) $ tval emptyRenderable ]
  in gridToRenderable g

-- Should this be exported by Layout?
type LegendItem = (String,Rect -> ChartBackend ())

mkLegend :: Maybe LegendStyle -> Double -> [LegendItem] -> Renderable (PickFn a)
mkLegend mls lm vals = case mls of
    Nothing -> emptyRenderable
    Just ls ->  case filter ((/="").fst) vals of
        []  -> emptyRenderable ;
        lvs -> addMargins (0,lm,lm,lm) $
                   setPickFn nullPickFn $ legendToRenderable (Legend ls lvs)

-- TODO: have to work out extra size here rather than hard code it!
extraSpace ::
  PolarLayout r t
  -> ChartBackend (Double, Double)
extraSpace _ = return (10,10)

minsizePolarLayout ::
  PolarLayout r t
  -> ChartBackend (Double, Double)
minsizePolarLayout pl = do
  (extraw, extrah) <- extraSpace pl
  return (extraw * 2, extrah * 2)

-- unit conversion
d2r :: RealFloat a => a -> a
d2r = ((pi/180.0) *)

r2d :: RealFloat a => a -> a
r2d = ((180.0/pi) *)

-- d2r :: RealFloat a => a -> Double
-- d2r a = (pi/180.0) * realToFrac a

-- Remove points (and plots) with 'invalid' points in them.
-- At present, this is just those points with radii < 0.
--
-- Note that this does not remove the corresponding legend    
-- (if all the points turn out to be invalid).
--    
-- TODO: can get
--    "scale by zero!  Halp!"
--  run-time errors (with PlotPoints at least). Where is this
--  coming from? (changing the fpts filter below to (>0) does not
--  help (and zero should be a valid radius). I think this may be
--  a radius issue in plotpoints and maybe such points should be filtered
--  out there (if true).
--
filterPlots :: (Num r, Ord r) => [Plot r t] -> [Plot r t]
filterPlots = filter (not . null . fst . _plot_all_points) . map noNeg
  where
    noNeg pp = 
      let pts = uncurry zip $ _plot_all_points pp
          fpts = filter ((>=0) . fst) pts
      in pp { _plot_all_points = unzip fpts }

renderPolarLayout :: 
  PolarLayout r t
  -> RectSize
  -> ChartBackend (PickFn a)
renderPolarLayout pl sz@(w,h) = do
  (extraw, extrah) <- minsizePolarLayout pl
  let center_x = w/2
      center_y = h/2
      
      pa = _polarlayout_axes pl
      
      radius = min (w - extraw) (h - extrah) / 2
      
      allPlots = _polarlayout_plots pl
      plots = _polar_axes_filter_plots pa allPlots
      points = (concat *** concat) $ unzip $ map _plot_all_points plots
      
      adata = _polar_axes_generate pa points
      
  case _polarlayout_plot_background pl of
    Nothing -> return ()
    Just fs -> withFillStyle fs $ 
      alignFillPath (arc' center_x center_y radius 0 (2*pi))
      >>= fillPath
  
  -- QUS: would it make sense to do the grid/axis within the clip
  --      region too?
  renderAxesAndGrid sz (center_x,center_y) radius pa adata
  withClipRegion (Rect (Point 0 0) (Point w h)) $
    mapM_ (renderSinglePlot sz radius (Just adata)) plots
  
  return nullPickFn

-- TODO:
--   if the radial axis is drawn, then it uses the location of
--   theta=0 (_polaraxes_theta_zero), but the radial labels are
--   likely drawn at a different angle. Shouldn't it be drawn
--   at the same angle as the labels?  
--  
renderAxesAndGrid ::
  (Double,Double)
  -- ^ width,height (display coordinates)
  -> (Double,Double)
  -- ^ center (display coordinates)
  -> Double
  -- ^ Max radius (display coordinates)
  -> PolarLayoutAxes r t
  -> PolarAxesData r t
  -> ChartBackend ()
renderAxesAndGrid sz (cx,cy) radius pa adata = do
  let conv = _polaraxes_viewport adata sz radius
      (_, rmax) = _polaraxes_r_range adata
  
      p0 = Point cx cy

      getR r = let (cx2,cy2) = conv (r,rAngle)
               in sqrt $ (cx2-cx)*(cx2-cx) + (cy2-cy)*(cy2-cy)

      cr = getR rmax
      rAngle = _polaraxes_theta_zero adata
      -- rmargin = _polar_theta_axis_margin pa
    
  -- Draw the grid lines
  withLineStyle (_polar_grid_style pa) $ do
    -- constant r
    forM_ (_polaraxes_r_grid adata) $ \r -> 
      let r' = getR r
      in strokePath $ arc' cx cy r' 0 (2*pi)
    
    -- constant theta
    forM_ (_polaraxes_t_grid adata) $ \theta ->
      let p1 = uncurry Point $ conv (rmax,theta)
      in alignStrokePoints [p0, p1] >>= strokePointPath

  -- theta axis
  withLineStyle (_polar_theta_axis_style pa) $
    strokePath $ arc' cx cy cr 0 (2*pi)

  -- radial axis
  withLineStyle (_polar_radial_axis_style pa) $
    let p1 = uncurry Point $ conv (rmax,rAngle)
    in alignStrokePoints [p0, p1] >>= strokePointPath

  -- axis labels
  -- TODO: improve positioning of labels
  withFontStyle (_polar_axes_label_style pa) $ do
    -- radial
    --
    -- TODO: using thetaLabelOffsets isn't quite right here - e.g.
    -- if the axis were horizontal we'd want HTA_Center VTA_Top
    -- rather than HTA_Left VTA_Center but leave as is for now
    forM_ (_polaraxes_r_labels adata) $ \(r,t,txt) -> 
      drawTextTheta conv (cx,cy) (r,t) txt 
   
    -- theta
    --
    -- rmargin is in pixels but need data coordinates for conv,
    -- so scale it
    --
    -- TODO: need to send this conversion routine in somehow
    --       to avoid typeclass constraints on the routine,
    --       or do this scaling elsewhere (should probably be included
    --       in the calculation of _polaraxes_t_labels)
    --
    -- let roff = (cr + rmargin) * rmax / cr
    let roff = rmax -- error "TODO: need to scale offset somehow"
    
    forM_ (_polaraxes_t_labels adata) $ \(theta,txt) -> 
      drawTextTheta conv (cx,cy) (roff,theta) txt

-- Represent label positions about the polar plot by what "zone" they are
-- in. The zone is based on quadrants, but I provide special cases for
-- positions close to n pi / 2 (n=0, 1, 2, 3). This is a quick hack to
-- improve label positioning but is not likely to be sufficient.
--

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
  ((r,t) -> (Double, Double))
  -- ^ convert from r,theta to plot coordinates
  -> (Double, Double)
  -- ^ center of the area, in display coordinates
  -> (r,t)
  -> String
  -> ChartBackend ()
drawTextTheta conv (cx,cy) p txt =
  let (lx,ly) = conv p
      dx = lx - cx
      dy = cy - ly -- note: y increases down
      (lxoff,lyoff) = thetaLabelOffsets $ atan2 dy dx
  in drawTextA lxoff lyoff (Point lx ly) txt
                        
-- | Convert a theta label value into the string to be displayed.
-- 
--   This is the default setting when the theta axis has a type of
--   @Double@ or @Degrees@.
showThetaLabelDegrees ::
  Int       -- ^ the number of \"ticks\" to show, @nmax@, assumed to be greater than 1.
  -> Int    -- ^ "number" of the current \"tick mark\"; varies from @0@ to @nmax-1@ inclusive.
  -> String -- ^ the @&#x03b8;@ value in degrees, including the degrees symbol
showThetaLabelDegrees n i = 
  let a = 360.0 :: Double
  in showD (a * fromIntegral i / fromIntegral n) ++ [chr 176]

-- | Convert a theta label value into the string to be displayed.
-- 
--   This is the default setting when the theta axis has a type of
--   @Radians@.
showThetaLabelRadians ::
  Int       -- ^ the number of \"ticks\" to show, @nmax@, assumed to be greater than 1.
  -> Int    -- ^ "number" of the current \"tick mark\"; varies from @0@ to @nmax-1@ inclusive.
  -> String -- ^ the @&#x03b8;@ value in units of @&#x03c0;@.
showThetaLabelRadians _ 0 = "0"
showThetaLabelRadians n i = 
  let piChar = chr 0x03c0

      piFrac :: Int -> Int -> String
      piFrac 1 1 = [piChar]
      piFrac 1 b = piChar : "/" ++ show b
      piFrac a 1 = show a ++ [piChar]
      piFrac a b = show a ++ piChar : "/" ++ show b

      j = 2*i
      g = gcd j n
  in if g == 1
     then piFrac j n
     else piFrac (j `div` g) (n `div` g)

-- | Render a single set of plot data onto a plot area of given size using
--   the given axes.
renderSinglePlot :: 
  RectSize 
  -> Double
  -- ^ maxmimum radius , in device units, to draw
  -> Maybe (PolarAxesData r t) -> Plot r t -> ChartBackend ()
renderSinglePlot sz radius (Just adata) p =
  {-
  let xr = optPairReverse xrev (0, w)
      yr = optPairReverse yrev (h, 0)
      -- yrange = if yrev then (0, h) else (h, 0)
      pmfn (x,y) = Point (mapv xr (_axis_viewport xaxis xr) x)
                         (mapv yr (_axis_viewport yaxis yr) y)
      mapv lims _ LMin       = fst lims
      mapv lims _ LMax       = snd lims
      mapv _    f (LValue v) = f v
  in _plot_render p pmfn
  -}
  let axConv' = _polaraxes_viewport adata
      axConv = axConv' sz radius
      conv (LValue a, LValue b) = uncurry Point $ axConv (a,b)
      conv _ = Point 0 0 -- TODO: how to handle invalid points?
      
  in _plot_render p conv
  
renderSinglePlot _ _ _ _ = return ()

-- Set up the lenses

$( makeLenses ''PolarLayout )
$( makeLenses ''PolarLayoutAxes )
$( makeLenses ''PolarAxesData )

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
-- for more information (it seems that this has now been fixed so we just need
-- a Chart release (>1.2) to upgrade the lens constraint so that the updated
-- diagrams code can be used).
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
-- > makeData :: LUT (Colour Double) -> IO [PlotPoints Double Double]
-- > makeData lut = do
-- >   let npts = 150
-- >       rand = replicateM npts (randomIO :: IO Double)
-- >       shapes = [ PointShapeCircle
-- >                , PointShapeCircle
-- >                , PointShapeCircle
-- >                , PointShapeCircle
-- >                , PointShapeCircle
-- >                , PointShapeCircle
-- >                , PointShapePolygon 3 True
-- >                , PointShapePolygon 4 True
-- >                , PointShapePolygon 5 True
-- >                , PointShapePolygon 6 True
-- >                , PointShapePlus
-- >                , PointShapeCross
-- >                , PointShapeStar
-- >                ]
-- >   r1 <- rand
-- >   r2 <- rand
-- >   r3 <- rand
-- >   shps <- replicateM npts (randomRIO (0,length shapes -1))
-- >   let rs = map (2*) r1
-- >       rads = zipWith (\ra rb -> sqrt (ra*100) * rb) rs r3
-- >       cols = map (flip withOpacity 0.4 . fromLUT lut) r2
-- >       thetas = map (2*pi*) r2
-- >       pitem (r,t,s,c,shp) = plot_points_style .~ pstyle s c shp
-- >                           $ plot_points_values .~ [(r,t)]
-- >                           $ def
-- >       pstyle s c shp = point_shape .~ shapes !! shp
-- >                      $ point_color .~ c
-- >                      $ point_radius .~ s
-- >                      $ point_border_color .~ opaque black
-- >                      $ point_border_width .~ 1
-- >                      $ def
-- >                    
-- >   return $ map pitem $ zip5 rs thetas rads cols shps
-- > 
-- > lPlot :: PlotLines Double Double
-- > lPlot =
-- >   let lvs = map (\r -> (r, 2*pi*r)) [0, 0.01 .. 2]
-- >   in plot_lines_values .~ [lvs]
-- >      $ plot_lines_style . line_color .~ withOpacity red 0.4
-- >      $ plot_lines_style . line_width .~ 2
-- >      $ def
-- > 
-- > bgFill, pFill :: FillStyle
-- > bgFill = solidFillStyle (opaque gray)
-- > pFill = solidFillStyle (withOpacity orange 0.4)
-- >       
-- > testPlot :: ToPlot p => [p Double Double] -> PolarLayout Double Double
-- > testPlot pps = 
-- >   polarlayout_title .~ "Polar plot"
-- >     $ polarlayout_background .~ bgFill
-- >     $ polarlayout_plot_background .~ Just pFill
-- >     $ polarlayout_plots .~ lPlot : map toPlot pps
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
--   - now that @PolarLayout@ is parameterized by @r@ and @t@, can we 
--     get back some of the functionality the earlier versions had
--     (customizability but also functionality, as the current code
--     seems like it is setting up things like the coordinate conversion
--     routines too early in the process)
--
--   - allow theta values to be given in radians or degrees
--
--   - fix up axis positioning and size calculation; this is mainly improving
--     the position of the axis tick labels and including this info in the
--     size calculations.
--
--   - how much can we generalize to other non-cartesian projections
--
