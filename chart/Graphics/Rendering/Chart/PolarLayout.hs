{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-} -- only needed to hide a defaulting warning

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.PolarLayout
-- Copyright   :  (c) Tim Docker 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Plot data using polar @(r, &#x03b8;)@ coordinates.
-- Should this be @Graphics.Rendering.Chart.Layout.Polar@?
--
-- I am more interested in seeing how to
-- expand Chart to more-complex projections such as those used in
-- Astronomy (<http://www.atnf.csiro.au/people/mcalabre/WCS/>),
-- but there is plenty of complication if we just stick to the
-- Earth (<http://www.progonos.com/furuti/MapProj/Normal/TOC/cartTOC.html>).
--
-----------------------------------------------------------------------------

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
module Graphics.Rendering.Chart.PolarLayout(
  
  -- * Example
  --
  -- $example
  
  -- * Notes
  --
  -- $notes
  
  -- * Symbols
  
  PolarLayout(..)
  , PolarLayoutAxes(..)
    
  -- * Rendering  
    
  , polarLayoutToRenderable
    
  -- * Lenses
    
  , polarlayout_background
  , polarlayout_plot_background
  , polarlayout_title
  , polarlayout_title_style
  , polarlayout_axes
  , polarlayout_plots
  , polarlayout_legend
  , polarlayout_margin
    
  , polar_grid_style
  , polar_theta_axis_style
  , polar_radial_axis_style
  , polar_axes_label_style
  , polar_margin
  , polar_ticklen
  , polar_axes_filter_plots    
  , polar_axes_generate
    
  )
       where

import Control.Arrow ((***))
import Control.Monad ((>=>), forM, forM_, when)
import Control.Lens

import Data.Char (chr)
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.List (foldl1')
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))

import Graphics.Rendering.Chart.Axis.Polar
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Graphics.Rendering.Chart.Grid (aboveN, besideN, gridToRenderable, tval, weights)
import Graphics.Rendering.Chart.Legend
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Utils (maybeM)

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

instance ToRenderable (PolarLayout r t) where
  toRenderable = setPickFn nullPickFn . polarLayoutToRenderable

-- | Render the data, axes, and labels.
--
--   At present the axes and grid are /always/ drawn first, and then the data.
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

-- Use the label \"180\" to guess the margins for the
-- plot, combined with the _polar_margin and
-- _polar_ticklen values        
--        
-- TODO: instead of _polar_ticklen, use _axis_label_gap        
--   BUT then need to have access to an AxisStyle record        
--        
-- TODO: perhaps the string used here (180 degrees) should        
--       be taken from the PolarLayoutAxes record rather
--       than being hard coded?        
guessMargins ::
  PolarLayoutAxes r t
  -> ChartBackend (Double, Double)
guessMargins pa = do
  let m = _polar_margin pa + _polar_ticklen pa
  (tw,th) <- withFontStyle (_polar_axes_label_style pa) $
             textDimension ("180" ++ [degSym])
  return (tw+m, th+m)
  
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
  (mx, my) <- guessMargins $ _polarlayout_axes pl
  return (mx*2, my*2)

r2d :: RealFloat a => a -> a
r2d = ((180.0/pi) *)

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
-- TODO: support asymmetric margins
--  
calculateArea ::
  (Double, Double)
  -- ^ Estimate of the margin width and height for the plot
  -> PlotBBox
  -> RectSize
  -> (Double, Double, Double, Rect)
  -- ^ Returns @(cx,cy,r,bbox)@ - the center of the plot, the
  --   maximum radius, and the bounding box of the plot,
  --   in device coordinates. The bounding box gives the smallest
  --   rectangle enclosing the plot itself (excluding ticks and
  --   labels); it was useful in testing so is left as a return
  --   value but may be removed in future revisions.
calculateArea (dx,dy) ((xl,yl),(xh,yh)) (w,h) = 
  let px = w - dx * 2
      py = h - dy * 2
      
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
      
      -- important to have y pixel values increasing from first to 
      -- the second point
      x1 = cx + xl * radius
      x2 = cx + xh * radius
      y1 = cy - yh * radius
      y2 = cy - yl * radius
      rect = Rect (Point x1 y1) (Point x2 y2)
  
  in (cx, cy, radius, rect)
  
-- The limits of the minimum box surrounding the plot     
-- itself (so ignoring ticks/margins/labels), given
-- in units of the radius, so ((-1,-1), (1,1))
-- is the full circle.
--
-- The coordinate system is as viewed on screen,
-- so X increases left to right and Y increases
-- bottom to top, which means that Y has to be
-- flipped to map it to device coordinates.
--
type PlotBBox = ((Double, Double), (Double, Double))
                               
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
  -> PlotBBox
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

-- TODO: refactor xxxBoundingBoxes and other code that uses this logic
--
-- Find the bounding boxes of the labels for the radial axis.
--
-- TODO: the boxes should account for the rotation used with these labels
--
radialBoundingBoxes ::
  ((r,t) -> Point)
  -> (Double,Double)
  -> Double
  -> FontStyle
  -> [(r,String)]
  -> t
  -- ^ The angle at which the radial axis labels are drawn
  -> ChartBackend [Rect]
radialBoundingBoxes pconv (cx,cy) margin lstyle tdata t0 =
  withFontStyle lstyle $ 
    -- TODO: use thetaLabelOffsets  or maybe need a radialLabelOffsets?
     -- let ang = r2d $ scaleT t0 -- angle in degrees
    forM tdata $ \(r,txt) ->
      let pos = pconv (r,t0)
          dx = p_x pos - cx
          dy = cy - p_y pos
          rscale = margin / sqrt (dx*dx + dy*dy)
          lpos = pvadd pos $ Vector (rscale*dx) (rscale*dy)
      -- There is no textDrawRectR yet, so assume an angle of 0          
      -- r <- textDrawRectR HTA_Centre VTA_Top ang lpos txt
      in textDrawRect HTA_Centre VTA_Top lpos txt

-- find the bounding boxes of the labels for the theta axis
thetaBoundingBoxes ::
  ((r,t) -> Point)
  -> (Double,Double)
  -> Double
  -> r
  -> FontStyle
  -> [(t,String)]
  -> ChartBackend [Rect]
thetaBoundingBoxes pconv (cx,cy) margin rmax lstyle rdata =
  withFontStyle lstyle $ 
    -- TODO: this copies code from drawTextTheta
    forM rdata $ \(theta,txt) -> 
      let pos = pconv (rmax,theta)
          dx = p_x pos - cx
          dy = cy - p_y pos -- note: y increases down
          rscale = margin / sqrt (dx*dx + dy*dy)
          (lxoff,lyoff) = thetaLabelOffsets $ atan2 dy dx
          lpos = pvadd pos $ Vector (rscale*dx) (-rscale*dy)
      in textDrawRect lxoff lyoff lpos txt

-- Find the rectangle that encloses all the rectangles
combineRectangles :: [Rect] -> Maybe Rect
combineRectangles rs = 
  let runion (Rect (Point ax1 ay1) (Point ax2 ay2))
             (Rect (Point bx1 by1) (Point bx2 by2))
        = let -- NOTE: 
              -- Once we have folded the first pair
              -- in rs then we know the a values are 
              -- ordered correctly (e.g. that ax2 >= ax2)
              -- but we can not take advantage of that
              -- to simplify things here
              xs = [ax1, ax2, bx1, bx2]
              ys = [ay1, ay2, by1, by2]
          in Rect (Point (minimum xs) (minimum ys))
                  (Point (maximum xs) (maximum ys))
        {- If we can guarantee the ordering
        = Rect (Point (min ax1 bx1) (min ay1 by1))
               (Point (max ax2 bx2) (max ay2 by2))
        -}
  in if null rs then Nothing else Just $ foldl1' runion rs

data PlotChange = 
  PC 
  (Maybe Double)
  -- Shift, in pixels, to apply to the center
  (Maybe Double)
  -- Excess, in pixels, that need to be changed by a
  -- change in the radius (i.e. left over after the
  -- shift has been applied)

-- Work out the shift/overlap between the border
-- rectangle and the current plot.
-- Note that this is intended for shifting/making
-- the plot smaller; it could, but does not currently,
-- work to make the plot larger (i.e. if it turns
-- out there is "spare" space to use).
--
compareBorders ::
  Double
  -- ^ minimum allowed position
  -> Double
  -- ^ maximum allowed position
  -> Double
  -- ^ minimum calculated position
  -> Double
  -- ^ maximum calculated position
  -> PlotChange
compareBorders bmin bmax pmin pmax = 
  let -- xshift is value to add to the center to make the
      -- edge x align with the border. If either
      --    lshift > 0
      --    rshift < 0
      -- then the plot overlaps the border.
      lshift = bmin - pmin
      rshift = bmax - pmax
      shift = (lshift + rshift) / 2
      -- Since this is floating-point equality, this should
      -- check for a tolerance, or just drop the idea of
      -- there being a separate indicator for 'no shift'
      mshift = if shift == 0 then Nothing else Just shift
      
      -- What is the overlap after applying the shift?
      -- If there still is any, need to rescale.
      lshift' = lshift - shift
      mscale = if lshift' > 0 then Just lshift' else Nothing
      
  in PC mshift mscale

shiftCenter :: 
  Double
  -- ^ The current center
  -> PlotChange
  -- ^ Result for compareBorders for this axis
  -> Double
  -- ^ The new center
shiftCenter cen (PC Nothing _)   = cen
shiftCenter cen (PC (Just dc) _) = cen + dc
      
rescaleRadius ::                                   
  (PlotChange, PlotChange)
  -- ^ The X and Y changes from compareBorders
  -> (Double, Double)
  -- ^ The width and height of the plot itself, in
  --   units of the radius
  -> Double
  -- ^ The current radius
  -> Double
  -- ^ The new radius value
rescaleRadius (PC _ Nothing, PC _ Nothing)  _ r = r
rescaleRadius (PC _ mx, PC _ my) (fx,fy) r = 
  let -- Calculate the new radius that creates a new
      -- plot that spans max (2*mx) (2*my) less pixels
      -- than the original plot (using a 0 if there is
      -- no shift). The factor of 2 is because the
      -- overlap is equal - ie on both sides.
      --       
      rx = (fx * r - 2 * fromMaybe 0 mx) / fx
      ry = (fy * r - 2 * fromMaybe 0 my) / fy
  in min rx ry
                                   
-- | Calculate the position and scaling used to draw the
--   plot based on the available space, label positions,
--   and data. This is an iterative solution, since you
--   need to know the scaling to work out the label positions,
--   and hence margins, but to know the scaling you need
--   to know the position. Here we perform one iteration
--   but do not check that the calculated values are
--   sufficient (e.g. due to rounding), since it's not
--   worth it until it turns out to be a problem.
--
-- NOTE:
--   the algorithm *always* takes into account the axis
--   labels even if they are not visible.
--
findPosition ::
  PolarLayoutAxes r t
  -> PolarAxesData r t
  -> PlotBBox
  -> RectSize
  -> ChartBackend (Double, Double, Double)
  -- ^ Returns the cx,cy and radius values, in
  --   device units.
findPosition pa adata bbox sz = do
  -- initial guess
  ms1 <- guessMargins pa
  let (cx1, cy1, radius1, brect1) = calculateArea ms1 bbox sz 
      
  -- Given this system, do the labels overlap the plot bounding box
  -- or is there more space?
  --    
  let conv   = polaraxes_viewport adata (cx1,cy1) radius1
      pconv  = uncurry Point . conv
  
      raxis  = _polaraxes_r adata
      taxis  = _polaraxes_theta adata
      
      (_, rmax) = _radialaxis_range raxis
      
      margin = _polar_margin pa

  rls <- radialBoundingBoxes pconv (cx1,cy1) margin
         (_polar_axes_label_style pa)
         (_radialaxis_labels raxis)
         (_polaraxes_r_angle adata)
  tls <- thetaBoundingBoxes pconv (cx1,cy1) margin rmax
         (_polar_axes_label_style pa)
         (_thetaaxis_labels taxis)
      
  {- DEBUG
  withLineStyle (dashedLine 1 [2,2] (opaque cyan)) $
    let mRect = Rect (Point msx msy) (Point (fst sz-msx) (snd sz-msy))
        (msx,msy) = ms1
    in alignStrokePath (rectPath mRect) >>= strokePath
  
  withLineStyle (dashedLine 1 [2,4] (opaque blue)) $
    let mRect = Rect (Point margin margin) (Point (fst sz-margin) (snd sz-margin))
    in alignStrokePath (rectPath mRect) >>= strokePath

  withLineStyle (solidLine 1 (opaque red)) $
    alignStrokePath (rectPath brect1) >>= strokePath

  withLineStyle (solidLine 1 (opaque red)) $ 
    forM_ (rls ++ tls) $ \rect ->
      alignStrokePath (rectPath rect) >>= strokePath
  -}
  
  -- combineRectangles is the bounding box of the plot + labels,
  -- so we can see if it overlaps the bounding box
  -- defined by _polar_margin_pa and see if a shift
  -- or rescale is needed.
  --
  -- As called we know that combineRectangles always
  -- returns a Just value, but I can not be bothered
  -- to use NonEmptyList to encode this constraint
  -- so leave as is for now.
  --
  case combineRectangles (brect1 : rls ++ tls) of
    
    Just (Rect (Point rx1 ry1) (Point rx2 ry2)) -> 
      let xchange = compareBorders margin (fst sz-margin) rx1 rx2
          ychange = compareBorders margin (snd sz-margin) ry1 ry2
      
          ((fx1,fy1),(fx2,fy2)) = bbox
          cx2 = shiftCenter cx1 xchange
          cy2 = shiftCenter cy1 ychange
          radius2 = rescaleRadius (xchange,ychange)
                    (fx2-fx1,fy2-fy1) radius1
                
      in return (cx2, cy2, radius2)
         
    _ -> return (cx1, cy1, radius1)

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
      
      taxis = _polaraxes_theta adata
      bbox = calcBBox (_thetaaxis_scale taxis) (_thetaaxis_range taxis)

  (cx, cy, radius) <- findPosition pa adata bbox sz    
  
  -- QUS: would it make sense to do the grid/axis within the clip
  --      region too?
  renderAxesAndGrid (cx,cy) radius pa adata 
    (_polarlayout_plot_background pl) (_polarlayout_background pl)
  withClipRegion (Rect (Point 0 0) (Point w h)) $
    mapM_ (renderSinglePlot (cx,cy) radius adata) plots
  
  return nullPickFn

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
  let conv = polaraxes_viewport adata (cx,cy) radius
      raxis = _polaraxes_r adata
      taxis = _polaraxes_theta adata
      scaleR = _radialaxis_scale raxis radius
      scaleT = _thetaaxis_scale taxis 
      (rmin, rmax) = _radialaxis_range raxis
      rmin' = scaleR rmin
      pconv = uncurry Point . conv
  
      p0 = Point cx cy

      -- TODO: clean up the repeated checks on whether there is
      --       a restricted theta range or not
      --
      mtrange = _thetaaxis_range taxis
      -- True if a restricted theta range is being displayed
      hasThetaRange = isJust mtrange
      (arcFn, arcFn') = if hasThetaRange
                        then (arcNeg', arc')
                        else (arc', arcNeg')
                         
      (tmin', tmax') = case mtrange of
        Just (tmin, tmax) -> (scaleT tmin, scaleT tmax)
        _ -> (0, 2*pi)

      (rPoints, rAngle0) = case mtrange of
        Just (tmin, tmax) ->
          let ps = pconv (rmax, tmin)
              pe = pconv (rmax, tmax)
          in if rmin' > 0
             then ([[ps, pconv (rmin,tmin)], [pe, pconv (rmin,tmax)]], tmin)
             else ([[ps, p0, pe]], tmin)
          
        _ ->
          let rAngle = _polaraxes_r_angle adata
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
        in case mtrange of
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
    forM_ (_radialaxis_grid raxis) $ \r -> 
      let r' = scaleR r
      in strokePath $ arcFn cx cy r' tmin' tmax'
    
    -- constant theta
    forM_ (_thetaaxis_grid taxis) $ \theta ->
      let pmin = pconv (rmin,theta)
          pmax = pconv (rmax,theta)
      in alignStrokePoints [pmin, pmax] >>= strokePointPath

  -- theta axis and tick marks
  withLineStyle (_polar_theta_axis_style pa) $ do
    when (rmin' > 0) $
      strokePath (arcFn cx cy rmin' tmin' tmax')
    strokePath (arcFn cx cy radius tmin' tmax')
    forM_ (_thetaaxis_ticks taxis) $ \theta -> do
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
    forM_ (_radialaxis_ticks raxis) $ \rt ->
      let ps = pconv (rt,rAngle0)
          pe = pvadd ps v1
      in alignStrokePoints [ps, pe] >>= strokePointPath
      
    maybeM () (\(_,tmax) -> 
                let p1' = pconv (rmax, tmax)
                    dx' = p_x p1' - cx
                    dy' = cy - p_y p1'
                    r' = ticklen / sqrt (dx'*dx' + dy'*dy')
                    v2 = Vector (dy'*r') (dx'*r')
        
                in forM_ (_radialaxis_ticks raxis) $ \rt ->
                     let ps = pconv (rt,tmax)
                         pe = pvsub ps v2
                     in alignStrokePoints [ps, pe] >>= strokePointPath)
      (_thetaaxis_range taxis)

  -- axis labels
  -- TODO: improve positioning of labels
  --       - should the angle be a user option - ie either force    
  --         horizontal or at the angle of the axis/perpendicular to it?
  --
  withFontStyle (_polar_axes_label_style pa) $ do
    -- radial
    --
    -- TODO: use thetaLabelOffsets  or maybe need a radialLabelOffsets?
    let t0 = _polaraxes_r_angle adata
        ang = r2d $ scaleT t0 -- angle in degrees
    forM_ (_radialaxis_labels raxis) $ \(r,txt) -> 
      let pos = pconv (r,t0)
          dx = p_x pos - cx
          dy = cy - p_y pos
          rscale = margin / sqrt (dx*dx + dy*dy)
          lpos = pvadd pos $ Vector (rscale*dx) (rscale*dy)
      in drawTextR HTA_Centre VTA_Top ang lpos txt
   
    -- theta
    --
    -- TODO: should these be rotated?
    forM_ (_thetaaxis_labels taxis) $ \(theta,txt) -> 
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
  let axConv = polaraxes_viewport adata (cx,cy) radius
      conv (LValue a, LValue b) = uncurry Point $ axConv (a,b)
      conv _ = Point 0 0 -- TODO: how to handle invalid points?
      
  in _plot_render p conv
  
-- Set up the lenses

$( makeLenses ''PolarLayout )
$( makeLenses ''PolarLayoutAxes )

-- Documentation
--
-- $example
--
-- This is based on the scatter-plot version from matplotlib,
-- <http://matplotlib.org/examples/pie_and_polar_charts/polar_scatter_demo.html>.
-- It is a bit /excessive/ for this document, and will be simplified at some
-- point.
--
-- <<docimages/polar-example1.svg>>
--
-- > import qualified Graphics.Rendering.Chart.Backend.Cairo as C
-- >
-- > import Control.Applicative ((<$>>))
-- > import Control.Lens
-- > import Control.Monad (replicateM, void)
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
-- >     $ polarlayout_plots .~ toPlot lPlot : map toPlot pps
-- >     $ polarlayout_margin .~ 10
-- >     $ def
-- > 
-- > makePlot :: IO ()
-- > makePlot = do
-- >   setStdGen $ mkStdGen 49 -- "repeatable" randomness ;-)
-- >   r <- toRenderable . testPlot <$> makeData cubeHelix0
-- >   void $ C.renderableToFile (C.FileOptions (400,400) C.SVG) r "polar.svg"
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
--   - improve the label positions
--
--   - how much can we generalize to other non-cartesian projections
--
