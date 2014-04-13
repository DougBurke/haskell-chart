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
  
  -- * Symbols
  
  PolarAxesData(..)
  , RadialAxisData(..)
  , ThetaAxisData(..)
  , PolarPlotValue(..)    
  , Radians(..)
  , Degrees(..)
  , ScaleRadialAxis
  , ScaleThetaAxis
    
  -- * Utility routines for creating and displaying axes
  --    
  , polaraxes_viewport
    
  -- | These may be removed.  
  , autoScaledAxes
  , showThetaLabelDegrees
  , showThetaLabelRadians
    
  -- * Lenses
    
  , polaraxes_r
  , polaraxes_theta
  , polaraxes_r_angle
    
  , radialaxis_visibility
  , radialaxis_scale
  , radialaxis_range
  , radialaxis_ticks
  , radialaxis_labels
  , radialaxis_grid
    
  , thetaaxis_visibility
  , thetaaxis_scale
  , thetaaxis_range
  , thetaaxis_ticks
  , thetaaxis_labels
  , thetaaxis_grid
  )
       where

import Control.Lens

import Data.Char (chr)
import Data.Default.Class

import Graphics.Rendering.Chart.Axis.Internal (scaleLinear, showD)
import Graphics.Rendering.Chart.Axis (AxisVisibility(..), PlotValue(..), autoScaledAxis)
import Graphics.Rendering.Chart.Utils (maximum0)

-- The degree symbol
degSym :: Char
degSym = chr 176

{-
NOTES:

In the middle of converting the code to an approach closer to the cartesian case
used by Layout/Axis/Plot. It is not immediately clear how close a match we can
get.

At present there is no analog of the AxisT record, and much of the information
about tick marks and grid location is hard coded (this is a loss compared
to the code prior to this rework).

-}

-- | What types can be plotted as a pair of polar (r,theta) 
--   coordinates.
--  
--   /TODO:/  
--  
--    - Should there be a radial and theta class
--      which provides a "make xxAxisData" function
--      which can either be used here or replace this
--      class? The idea is that you can easily write
--      instances for the same radial type but different  
--      theta types; at present this requires repeated
--      code.  
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
    _polaraxes_r = 
       RadialAxisData { 
         _radialaxis_visibility = def
         , _radialaxis_scale = radialScale
         , _radialaxis_range = (rMin, rMax)
         , _radialaxis_ticks = rtickvs
         , _radialaxis_labels = zip rtickvs $ map showD rlabelvs
         , _radialaxis_grid = frgridvs
         }
       
    , _polaraxes_theta =
         ThetaAxisData {
           _thetaaxis_visibility = def
           , _thetaaxis_scale = thetaScale
           , _thetaaxis_range = Nothing
           , _thetaaxis_ticks = ttickvs 
           , _thetaaxis_labels = tlabelvs
           , _thetaaxis_grid = tgridvs
           }
         
    , _polaraxes_r_angle = rAngle
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
--   type but extended since there are two components
--   to be concerned with here.
--
data PolarAxesData r t =
  PolarAxesData 
  {
    _polaraxes_r :: RadialAxisData r
  , _polaraxes_theta :: ThetaAxisData t
  , _polaraxes_r_angle :: t
    -- ^ The angle at which the radial axis is drawn
    --   (used when the _thetaaxis_range field is
    --    @None@).
  }

-- | The basic data associated with a radial axis.
data RadialAxisData r =
  RadialAxisData 
  {
    _radialaxis_visibility :: AxisVisibility
    -- ^ Visibility of the axis components 
    --   (currently ignored)
    
  , _radialaxis_scale :: Double -> r -> Double
    -- ^ Convert the radial coordinate to a pixel value. The
    --   first argument is the maximum radius, in pixel units.
    
  , _radialaxis_range :: (r,r)  
    -- ^ The minimum and maximum r shown.
    
  , _radialaxis_ticks :: [r]
  , _radialaxis_labels :: [(r, String)]
    -- ^ These labels are draw at either the 
    --   `_polaraxes_r_angle` angle or the
    --   first `_thetaaxis_range` value.
    
  , _radialaxis_grid :: [r]
    -- ^ This is assumed not to include the outer radius.
  
  }

data ThetaAxisData t =
  ThetaAxisData 
  {
    _thetaaxis_visibility :: AxisVisibility
    -- ^ Visibility of the axis components 
    --   (currently ignored)
    
  , _thetaaxis_scale :: t -> Double
    -- ^ Convert the theta coordinate to radians, measured
    --   clockwise from the horizontal.
    --
  
  , _thetaaxis_range :: Maybe (t,t) 
    -- ^ The minimum and maximum theta shown, if not the full circle.
    
  , _thetaaxis_ticks :: [t]
  , _thetaaxis_labels :: [(t, String)]
  , _thetaaxis_grid :: [t]

  }

-- This used to be in PolarAxesData but pulled out
-- as it is a fixed function of the r and theta scaling
-- routines.
--
-- | Convert a data value @(r, &#x03b8;)@ into
--   device coordinates.
polaraxes_viewport :: 
  PolarAxesData r t
  -> (Double, Double) 
  -- ^ Center of the plot, in device units.
  -> Double 
  -- ^ Maximum radius of the plot, in device units.
  -> (r,t) 
  -- ^ Coordinate to convert.
  -> (Double, Double)
  -- ^ Position of the point, in device units.
polaraxes_viewport adata (cx,cy) radius (r,t) =  
  let r' = _radialaxis_scale (_polaraxes_r adata) radius r
      t' = _thetaaxis_scale (_polaraxes_theta adata) t
  in ( cx + r' * cos t'
     , cy + r' * sin t' ) 
      
-- unit conversion
d2r :: RealFloat a => a -> a
d2r = ((pi/180.0) *)

r2d :: RealFloat a => a -> a
r2d = ((180.0/pi) *)

-- d2r :: RealFloat a => a -> Double
-- d2r a = (pi/180.0) * realToFrac a

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
  in showD (a * fromIntegral i / fromIntegral n) ++ [degSym]

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

-- Set up the lenses

$( makeLenses ''PolarAxesData )
$( makeLenses ''RadialAxisData )
$( makeLenses ''ThetaAxisData )

