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

-- TODO (incomplete list):
--
--  - should the PolarLayoutAxes be split into 1D axes, since we can
--    use a common viewport routine once we have the radial- and theta-
--    scaling routines
--
--  - should the radial axis be drawn when a subset of the theta range
--    is drawn and the theta axis is being displayed? Or should the
--    radial parts be considered part of the radial axis display?
--    I am now leaning to the latter, since this seems to "solve"
--    the issue of having the axis drawn twice.
--
--  - the radial axis angle and tick-lable angle should be the same
--
--  - radial axis labels should be centered/below the axis and at the
--    same angle as the axis?
--
--  - there is a lot of logic in renderAxesAndGrid to determine where
--    to draw the axes; should this have already been calculated when
--    generating the PolarAxesData structure, and the render routine
--    just plots up the values?
--
--  - how to fill an annulus (e.g. circle(x,y,r1)-circle(x,y,r2) with r1>r2)?
--
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
  -- , polar_theta_axis_reverse
  , polar_grid_style
  , polar_theta_axis_style
  , polar_radial_axis_style
  , polar_axes_label_style
  , polar_margin
  , polar_ticklen
  , polar_axes_filter_plots    
  , polar_axes_generate
    
  , polaraxes_r_scale
  , polaraxes_theta_scale
  , polaraxes_r_range
  , polaraxes_theta_range
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
import Control.Monad ((>=>), forM_, when)
import Control.Lens

import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.Char (chr)
import Data.Maybe (isJust)
import Data.Monoid ((<>))

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
import Graphics.Rendering.Chart.Utils (maximum0, maybeM)

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
  
  , _polar_margin :: Double
    -- ^ The distance between the axis and the labels,
    --   in display coordinates
  , _polar_ticklen :: Double
    -- ^ Length of the tick marks,
    --   in device coordinates. Positive values are drawn outside
    --   the plot (theta) or clockwise (radial).

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
    , _polar_theta_axis_reverse = False
  -}
    { _polar_grid_style = dashedLine 1 [2,4] $ opaque lightgrey
    , _polar_theta_axis_style = solidLine 1 $ opaque black
    , _polar_radial_axis_style = solidLine 0 transparent 
    , _polar_axes_label_style = def
    , _polar_margin = 5
    , _polar_ticklen = 3
                                 
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

-- TODO: should this limit the r and theta ranges?
autoScaledAxes :: 
  (RealFloat r, RealFloat t)
  => ScaleRadialAxis r
  -> ScaleThetaAxis t
  -> Double
  -- ^ Angle (in radians, measured clockwise from the X axis)
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

      -- TODO:
      --  Resolve whether we need to filter the output of scaleR/T.
      --  Ideally we would not, but it is possible that the scaling
      --  routines used for the 1D case may not quite match what we
      --  want.
      --
      -- SHOULD this filtering be applied to all the outputs of scaleR?
      --
      -- we do not want grid lines at r=rmin and r=rmax
      frgridvs = filter (\r -> r > rMin && r < rMax) rgridvs

      -- scale between user and screen coordinates
      --
      -- TODO: sort out nameing between these and scaleT/R arguments to the
      --       function.
      thetaScale = if tRev then (tZero +) . realToFrac else (tZero -) . realToFrac
      radialScale radius r = realToFrac r * radius / realToFrac rMax
      
      -- can we use _polar_radial_axis_offset (or move this info
      -- around so we can use it)?
      rAngle = d2r (45.0/2)

      (ttickvs, tlabelvs, tgridvs) = scaleT ts
      
  in PolarAxesData {
      _polaraxes_r_scale = radialScale
    , _polaraxes_theta_scale = thetaScale
                            
    , _polaraxes_r_range = (rMin, rMax)
    , _polaraxes_theta_range = Nothing
    , _polaraxes_theta_zero = 0
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
  
-- | This is intended to be similar to the `AxisData`
--   type but is somewhat of a grab-bag of functionality
--   at this time, in part because there is currently no 
--   equivalent of `AxisT`.  
--  
--   TODO:
--    - With the removal of the viewport routine, should
--      this be simplified so that the axis-specific  
--      routines (e.g. radial or theta) are handled  
--      by individual structures and this is just a  
--      container for them? The idea being that it  
--      is easier to re-use the setup for one of the  
--      axes with different types of the other?  
--  
data PolarAxesData r t =
  PolarAxesData 
  {
    _polaraxes_r_scale :: Double -> r -> Double
    -- ^ Convert the radial coordinate to a pixel value. The
    --   first argument is the maximum radius, in pixel units.
    
  , _polaraxes_theta_scale :: t -> Double
    -- ^ Convert the theta coordinate to radians, measured
    --   clockwise from the horizontal.
    --
  
  , _polaraxes_r_range :: (r,r)  
    -- ^ The minimum and maximum r shown.
    
  , _polaraxes_theta_range :: Maybe (t,t) 
    -- ^ The minimum and maximum theta shown, if not the full circle.
    
  , _polaraxes_theta_zero :: t
    -- ^ Angle at which the radial axis is drawn.
    
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

-- This used to be in PolarAxesData but pulled out
-- as it is a fixed function of the r and theta scaling
-- routines.
--
_polaraxes_viewport :: 
  PolarAxesData r t
  -> (Double, Double) 
  -- ^ center of the plot, in device units
  -> Double 
  -- ^ maximum radius of the plot, in device units 
  -> (r,t) 
  -> (Double, Double)
_polaraxes_viewport adata (cx,cy) radius (r,t) =  
  let r' = _polaraxes_r_scale adata radius r
      t' = _polaraxes_theta_scale adata t
  in ( cx + r' * cos t'
     , cy + r' * sin t' ) 
      
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

-- QUESTION: what is the meaning of this? I'm not sure 
-- how to calculate any sort of a meaningful value without
-- access to the data values. For now just use a simple
-- heuristic, based on the margin size and the size of
-- the label \"180\".
--        
minsizePolarLayout ::
  PolarLayout r t
  -> ChartBackend (Double, Double)
minsizePolarLayout pl = do
  let pa = _polarlayout_axes pl
      m = _polar_margin pa
  (tw, th) <- withFontStyle (_polar_axes_label_style pa) $
              textDimension "180"
  return ((tw+m) * 2, (th+m) * 2)

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

{-
The code below, to calculate the coordinates of (r=0,theta=0)
in device untis, seems overly complicated. This is in part
because of several different coordinate systems, including
switching between measuring angles clockwise and anti-clockwise
and whether the Y axis is measured increasing up or down, but
it I also think I've missed a simple trick to simplify a
lot of the range code; that is, the code that calculates the
extent of a plot (when the theta range is limited), in
units of the radius, and the mapping to pixel coordinates,
including centering of the plot. This is before thinking
about accurate calculation of the margins, since the plot
size determines the margins, but you can't calculate the
plot size without knowing the margins.
-}

-- | Calculate the origin and maximum radius of the polar plot.
--
--   TODO:
--    
--   - once the original guess at the size has been calculated,    
--     should the label sizes be checked (now that their    
--     positions are known) and the size re-calculated?    
--     This would then let cases where a long radial label
--     overlaps the edge to be caught, at the expense of
--     some potentially iterative loop (hopefully it would    
--     converge quickly, but there's always the possibility    
--     of some strange oscillation that would have to be
--     guarded against).    
--    
calculateArea ::
  PolarLayoutAxes r t
  -> PolarAxesData r t
  -> RectSize
  -> ChartBackend (Double, Double, Double, Double, Double)
  -- ^ Returns @(cx,cy,r,mx,my)@ - the center of the plot, the
  --   maximum radius, and the margins from the left/bottom
  --   edge, all in device coordinates. The margins were useful
  --   in debugging and left in for now. Perhaps I should
  --   just return a @Rect@ representing the bounding-box of the
  --   plot itself within the @RectSize@ area.
calculateArea pa adata (w,h) = do  
  let m = _polar_margin pa
  (tw, th) <- withFontStyle (_polar_axes_label_style pa) $
              textDimension "180"
  
  let dx = tw + m
      dy = th + m
      px = w - dx * 2
      py = h - dy * 2
      
      ((xl,yl),(xh,yh)) = calcBBox (_polaraxes_theta_scale adata) (_polaraxes_theta_range adata)
      
      -- work out the radius given the aspect ratio of
      -- the plot bounding box and the rectangle
      
      -- bx and by give the bounding box in units of the radius
      -- so the radius is the minimum of px/bx and py/by
      (bx, by) = (xh-xl, yh-yl)
      rx = px/bx
      ry = py/by
      radius = min rx ry
      
      -- Calculate the center using the bounding-box coordinates
      -- and then shift the 'longer' axis so that the plot
      -- is centered.
      xcen = -xl * radius + dx
      ycen =  yl * radius + dy + py
      (cx, cy) = 
        if rx <= ry
        then (xcen, ycen - ((py - by * radius) / 2))
        else (xcen + ((px - bx * radius) / 2), ycen)
      
      -- TODO: support asymmetric margins
  
  return (cx, cy, radius, dx, dy)
  
-- TODO: 
--    - if have a partial r range (ie rmin > 0) then  
--      could restrict to just the plot range (i.e.  
--      no longer assume that r=0 is included)
--  
calcBBox :: 
  (t -> Double)
  -- ^ Convert the theta coordinate into radians
  -> Maybe (t,t) 
  -- ^ The theta range of the plot (if @Nothing@ then use 0 to 2pi)
  -> ((Double, Double), (Double,Double))
  -- ^ Coordinates of the lower-left and upper-right corners
  --   of the bounding box, using a normalized coordinate system
  --   ranging from -1 to 1 (X increasing to the right and Y
  --   increasing UP).
calcBBox _    Nothing = ((-1,-1), (1,1))
calcBBox conv (Just (tmin,tmax)) = 
  let -- conv converts to a clockwise value so need to
      -- convert back to anti-clockwise
      (tmin', tmax') = (-conv tmin, -conv tmax)
      (x1, x2) = (cos tmin', cos tmax')
      (y1, y2) = (sin tmin', sin tmax')
      zs = calcCardinal tmin' tmax'
      xs = [0, x1, x2] ++ xranges zs
      ys = [0, y1, y2] ++ yranges zs
      (xl, xh) = (minimum xs, maximum xs)
      (yl, yh) = (minimum ys, maximum ys)
  in ((xl,yl), (xh,yh))
  
-- | Return +/-1 values for each cardinal point that is
--   included in the zone list.
--
--   TODO: is the coordinate system for yranges correct?
xranges, yranges :: [Zone] -> [Double]
xranges [] = []
xranges (x:xs) = let rest = xranges xs
                 in case x of
                   ZR -> 1 : rest
                   ZL -> -1 : rest
                   _  -> rest
yranges [] = []
yranges (y:ys) = let rest = yranges ys
                 in case y of
                   ZT -> 1 : rest
                   ZB -> -1 : rest
                   _  -> rest
  
-- | Given the start and end angles of the plot, what
--   cardinal angles are included in the range?
--
--   This mixes up zones and cardinal points, and - since it uses
--   getZone - is only approximate
--   
calcCardinal :: 
  Double 
  -- ^ Start angle, in radians, measured anti-clockwise from the horizontal.
  -> Double 
  -- ^ End angle, in radians, measured anti-clockwise from the horizontal.
  -> [Zone]
calcCardinal tmin tmax =
  let z1 = getZone tmin
      z2 = getZone tmax
      
      -- Since theta is periodic, can not rely on
      -- tmin' <= tmax' to determine whether the
      -- clockwise rotation from min to max is
      -- < 90 degrees or > 270 degrees, so use
      -- the absolute Y coordinate instead.
      --
      as1 = abs $ sin tmin
      as2 = abs $ sin tmax
      
      cardinal = [ZR, ZT, ZL, ZB]
  in if z1 == z2
        -- Does the region stay in this zone/quadrant
        -- (separation < 90 degrees) or cover the
        -- all the quadrants (separation > 270 degrees).
        -- TODO: the following could be cleaned up but
        --       left as is to retain the logic
     then if (z1 < ZT && as1 <= as2) || 
             (z1 >= ZT && z1 < ZL && as1 >= as2) ||
             (z1 >= ZL && z1 < ZB && as1 <= as2) ||
             (z1 >= ZB && as1 >= as2)
          then []
          else cardinal
     else -- if z1 > z2 then we have to cross theta=0 to get
          -- from tmin to tmax
       if z1 > z2
          then takeWhile (<=z2) cardinal ++ dropWhile (<z1) cardinal
          else takeWhile (<=z2) $ dropWhile (<z1) cardinal

renderPolarLayout :: 
  PolarLayout r t
  -> RectSize
  -> ChartBackend (PickFn a)
renderPolarLayout pl sz@(w,h) = do
  let pa = _polarlayout_axes pl
      allPlots = _polarlayout_plots pl
      plots = _polar_axes_filter_plots pa allPlots
      points = (concat *** concat) $ unzip $ map _plot_all_points plots
      adata = _polar_axes_generate pa points
      
  (cx, cy, radius, _, _) <- calculateArea pa adata sz 
      
  -- QUS: would it make sense to do the grid/axis within the clip
  --      region too?
  renderAxesAndGrid (cx,cy) radius pa adata 
    (_polarlayout_plot_background pl) (_polarlayout_background pl)
  withClipRegion (Rect (Point 0 0) (Point w h)) $
    mapM_ (renderSinglePlot (cx,cy) radius adata) plots
  
  return nullPickFn

-- TODO:
--   if the radial axis is drawn, then it uses the location of
--   theta=0 (_polaraxes_theta_zero), but the radial labels are
--   likely drawn at a different angle. Shouldn't it be drawn
--   at the same angle as the labels?  
--  
renderAxesAndGrid ::
  (Double,Double)
  -- ^ center (display coordinates)
  -> Double
  -- ^ Max radius (display coordinates)
  -> PolarLayoutAxes r t
  -> PolarAxesData r t
  -> Maybe FillStyle
  -- ^ How to fill the background of the plot area
  -> FillStyle
  -- ^ The fill style for the whole layout (it is
  --   only needed as a hack for plots with a full
  --   theta range but a limited radial range).
  -> ChartBackend ()
renderAxesAndGrid (cx,cy) radius pa adata mbg bfs = do
  let conv = _polaraxes_viewport adata (cx,cy) radius
      scaleR = _polaraxes_r_scale adata radius
      scaleT = _polaraxes_theta_scale adata
      (rmin, rmax) = _polaraxes_r_range adata
      rmin' = scaleR rmin
      pconv = uncurry Point . conv
  
      p0 = Point cx cy

      -- TODO: clean up the repeated checks on whether there is
      --       a restricted theta range or not
      --
      -- True if a restricted theta range is being displayed
      hasThetaRange = isJust (_polaraxes_theta_range adata) 
      (arcFn, arcFn') = if hasThetaRange
                        then (arcNeg', arc')
                        else (arc', arcNeg')
                         
      (tmin', tmax') = case _polaraxes_theta_range adata of
        Just (tmin, tmax) -> (scaleT tmin, scaleT tmax)
        _ -> (0, 2*pi)

      (rPoints, rAngle0) = case _polaraxes_theta_range adata of
        Just (tmin, tmax) ->
          let ps = pconv (rmax, tmin)
              pe = pconv (rmax, tmax)
          in if rmin' > 0
             then ([[ps, pconv (rmin,tmin)], [pe, pconv (rmin,tmax)]], tmin)
             else ([[ps, p0, pe]], tmin)
          
        _ ->
          let rAngle = _polaraxes_theta_zero adata
              p1 = pconv (rmax,rAngle)
          in if rmin' > 0
             then ([[p1, pconv (rmin,rAngle)]], rAngle)
             else ([[p1, p0]], rAngle)
  
      -- TODO: work in progress; the remaining problem is to
      --       create an annulus (i.e. when the full theta range
      --       is displayed but only a subset of the radius).
      --
      --       The 'solution' is to fill two circles, with the
      --       inner circle matching the background area, but
      --       this is a hack that should be replaced.
      --
      (bPath, mbPath) = 
        let aout = arcFn cx cy radius tmin' tmax'
            ain  = arcFn' cx cy rmin' tmax' tmin'
        in case _polaraxes_theta_range adata of
          Just _ -> 
            if rmin' > 0
            then (aout <> ain, Nothing)
            else (moveTo' cx cy <> aout, Nothing)
          _ -> if rmin' > 0
               then (aout, Just ain)
               else (aout, Nothing)

      margin = _polar_margin pa
      ticklen = _polar_ticklen pa
      
  -- Fill in the plot area?
  case mbg of
    Nothing -> return ()
    Just fs -> do
      -- TODO:
      -- I do not know how to create a filled annulus,
      -- so I "fake" it by filling in the inner circle
      -- with the general background fill style.
      withFillStyle fs $ alignFillPath bPath >>= fillPath
      maybeM () (\bp -> withFillStyle bfs (alignFillPath bp >>= fillPath)) mbPath
  
  -- Draw the grid lines
  withLineStyle (_polar_grid_style pa) $ do
    -- constant r
    forM_ (_polaraxes_r_grid adata) $ \r -> 
      let r' = scaleR r
      in strokePath $ arcFn cx cy r' tmin' tmax'
    
    -- constant theta
    forM_ (_polaraxes_t_grid adata) $ \theta ->
      let pmin = pconv (rmin,theta)
          pmax = pconv (rmax,theta)
      in alignStrokePoints [pmin, pmax] >>= strokePointPath

  -- theta axis and tick marks
  withLineStyle (_polar_theta_axis_style pa) $ do
    when (rmin' > 0) $
      strokePath (arcFn cx cy rmin' tmin' tmax')
    strokePath (arcFn cx cy radius tmin' tmax')
    forM_ (_polaraxes_t_ticks adata) $ \theta -> do
      let p1 = pconv (rmax,theta)
          p2 = pconv (rmin,theta)
          v = Vector (dx*r) (dy*r) 
          dx = p_x p1 - cx
          dy = p_y p1 - cy
          r = ticklen / sqrt (dx*dx + dy*dy)
      alignStrokePoints [p1, pvadd p1 v] >>= strokePointPath
      when (rmin' > 0) $
        alignStrokePoints [p2, pvsub p2 v] >>= strokePointPath

  -- radial axis and tick marks
  --
  withLineStyle (_polar_radial_axis_style pa) $ do
    let p1 = pconv (rmax,rAngle0)
        dx = p_x p1 - cx
        dy = cy - p_y p1
        r = ticklen / sqrt (dx*dx + dy*dy)
        v1 = Vector (dy*r) (dx*r) 
        
    forM_ rPoints $ alignStrokePoints >=> strokePointPath
    forM_ (_polaraxes_r_ticks adata) $ \rt ->
      let ps = pconv (rt,rAngle0)
          pe = pvadd ps v1
      in alignStrokePoints [ps, pe] >>= strokePointPath
      
    maybeM () (\(_,tmax) -> 
                let p1' = pconv (rmax, tmax)
                    dx' = p_x p1' - cx
                    dy' = cy - p_y p1'
                    r' = ticklen / sqrt (dx'*dx' + dy'*dy')
                    v2 = Vector (dy'*r') (dx'*r')
        
                in forM_ (_polaraxes_r_ticks adata) $ \rt ->
                     let ps = pconv (rt,tmax)
                         pe = pvsub ps v2
                     in alignStrokePoints [ps, pe] >>= strokePointPath)
      (_polaraxes_theta_range adata)

  -- axis labels
  -- TODO: improve positioning of labels
  --       - should the angle be a user option - ie either force    
  --         horizontal or at the angle of the axis/perpendicular to it?
  --
  withFontStyle (_polar_axes_label_style pa) $ do
    -- radial
    --
    -- TODO: use thetaLabelOffsets  or maybe need a radialLabelOffsets?
    forM_ (_polaraxes_r_labels adata) $ \(r,t,txt) -> 
      let pos = pconv (r,t)
          ang = r2d $ scaleT t -- angle in degrees
          dx = p_x pos - cx
          dy = cy - p_y pos
          rscale = margin / sqrt (dx*dx + dy*dy)
          lpos = pvadd pos $ Vector (rscale*dx) (rscale*dy)
      in drawTextR HTA_Centre VTA_Top ang lpos txt
   
    -- theta
    --
    -- TODO: should these be rotated?
    forM_ (_polaraxes_t_labels adata) $ \(theta,txt) -> 
      drawTextTheta conv (cx,cy) (rmax,theta) margin txt

-- Represent label positions about the polar plot by what "zone" they are
-- in. The zone is based on quadrants, but I provide special cases for
-- positions close to n pi / 2 (n=0, 1, 2, 3). This is a quick hack to
-- improve label positioning but is not likely to be sufficient.
--
-- It mixes up two concepts - the cardinal locations and the quadrants
-- of a circle - and was originally created for calculating the
-- alignment of the theta labels, but is now also being used in
-- calculating the plot position/size.    
--    
data Zone =
  {-
  ZR | ZT | ZL | ZB -- the four "cardinal" locations (right, top, left, or bottom)
  |
  ZQ1 | ZQ2 | ZQ3 | ZQ4 -- the four quadrants
  -}
  ZR | ZQ1 | ZT | ZQ2 | ZL | ZQ3 | ZB | ZQ4
  deriving (Eq, Ord)
           
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
  -> Double
  -- ^ margin, to add to the vector from the center to (r,t)
  --   before drawing the label
  -> String
  -> ChartBackend ()
drawTextTheta conv (cx,cy) p margin txt =
  let (lx,ly) = conv p
      dx = lx - cx
      dy = cy - ly -- note: y increases down
      r  = margin / sqrt (dx*dx + dy*dy)
      lx' = lx + dx * r
      ly' = ly - dy * r -- TODO: correct size?
      (lxoff,lyoff) = thetaLabelOffsets $ atan2 dy dx
  in drawTextA lxoff lyoff (Point lx' ly') txt
                        
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
  (Double,Double)
  -- ^ center (display coordinates)
  -> Double
  -- ^ maxmimum radius , in device units, to draw
  -> PolarAxesData r t
  -> Plot r t 
  -> ChartBackend ()
renderSinglePlot (cx,cy) radius adata p =
  let axConv = _polaraxes_viewport adata (cx,cy) radius
      conv (LValue a, LValue b) = uncurry Point $ axConv (a,b)
      conv _ = Point 0 0 -- TODO: how to handle invalid points?
      
  in _plot_render p conv
  
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
