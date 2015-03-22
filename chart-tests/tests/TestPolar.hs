-- | Test out the polar plots
-- 

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestPolar where 

import Control.Arrow (second)
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.Maybe (catMaybes)
import Data.Tuple (swap)
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Axis.Internal (scaleLinear, showD)

-- import Utils

bgFill, pFill :: FillStyle
bgFill = solidFillStyle (opaque gray)
pFill = solidFillStyle (withOpacity orange 0.4)

toRad :: (Double,Double) -> (Double,Radians)
toRad = second Radians

lPlot :: PlotLines Double Double
lPlot =
  let lvs = map (\r -> (r, 2*pi*r)) [0, 0.01 .. 2]
  in plot_lines_values .~ [lvs]
     $ plot_lines_style . line_color .~ withOpacity red 0.4
     $ plot_lines_style . line_width .~ 2
     $ plot_lines_title .~ "line"
     $ def

{-
TODO: use lens to avoid some of the repitition we have

rlPlot :: PlotLines Double Radians
rlPlot = let [lvs] = _plot_lines_values lPlot
         in lPlot { _plot_lines_values = [map toRad lvs] }
-}
         
-- The points go out to 2.5 which is the maximum radius chosen for the
-- plot. This means that points will extend outside the plot. This
-- is intentional, since I have not decided how this should be handled,
-- such as increase the radius or fancy clipping, since it's not easy.
-- 

mkPlots :: (Double -> a) -> [PlotPoints Double a]
mkPlots f = 
  let shapes = [ PointShapeCircle, PointShapePolygon 4 True, PointShapeStar ]
      cols = [ orange, green, cyan ]
      nplots = length cols
      pshape (shp,col) = point_shape .~ shp
                         $ point_radius .~ 10
                         $ point_color .~ withOpacity col 0.4
                         $ point_border_color .~ opaque black
                         $ point_border_width .~ 1
                         $ def
                         
      pts i = let a = pi / 4 + 2 * pi * fromIntegral i / fromIntegral nplots
              in zip [0.25, 0.5 .. 2.5] $ repeat (f a)
      s i shp col = plot_points_style .~ pshape (shp,col)
                    $ plot_points_values .~ pts i
                    $ plot_points_title .~ "data " ++ show (i+1)
                    $ def
      
  in zipWith3 s [0..] shapes cols

pPlots :: [PlotPoints Double Double]
pPlots = mkPlots id

rPlots :: [PlotPoints Double Radians]
rPlots = mkPlots Radians

dPlots :: [PlotPoints Double Degrees]
dPlots = mkPlots Degrees

chart1 :: 
  Bool     -- ^ display the background
  -> Bool  -- ^ display the legend
  -> Renderable ()
chart1 b l = toRenderable $ polarLayout b l
        
chart2 :: 
  Bool     -- ^ display the background
  -> Bool  -- ^ display the legend
  -> Renderable ()
chart2 b l = toRenderable $ polarLayoutRadians b l
        
chart3 :: 
  Bool     -- ^ display the background
  -> Bool  -- ^ display the legend
  -> Renderable ()
chart3 b l = toRenderable $ polarLayoutDegrees b l
        
polarLayout :: Bool -> Bool -> PolarLayout Double Double
polarLayout b l = 
  let mlegend = if l then Just def else Nothing
      mbgnd = if b then Just pFill else Nothing
  in polarlayout_title .~ "Polar plot"
     $ polarlayout_background .~ bgFill
     $ polarlayout_plot_background .~ mbgnd
     $ polarlayout_plots .~ toPlot lPlot : map toPlot pPlots
     $ polarlayout_margin .~ 10
     $ polarlayout_legend .~ mlegend
     $ def

polarLayoutRadians :: Bool -> Bool -> PolarLayout Double Radians
polarLayoutRadians b l = 
  let mlegend = if l then Just def else Nothing
      mbgnd = if b then Just pFill else Nothing
  in polarlayout_title .~ "Polar plot 2"
     $ polarlayout_background .~ bgFill
     $ polarlayout_plot_background .~ mbgnd
     $ polarlayout_plots .~ map toPlot rPlots -- toPlot rlPlot : map toPlot rPlots
     $ polarlayout_margin .~ 10
     $ polarlayout_legend .~ mlegend
     $ polarlayout_axes .~ (polar_grid_style .~ (solidLine 1 (opaque orange)) 
                            $ polar_theta_axis_style .~ (solidLine 0 (opaque black))
                            $ def)
     $ def

-- Should the radial axis be drawn at theta=0 or the same angle used for
-- the labels?
polarLayoutDegrees :: Bool -> Bool -> PolarLayout Double Degrees
polarLayoutDegrees b l = 
  let mlegend = if l then Just def else Nothing
      mbgnd = if b then Just pFill else Nothing
  in polarlayout_title .~ "Polar plot 3"
     $ polarlayout_background .~ bgFill
     $ polarlayout_plot_background .~ mbgnd
     $ polarlayout_plots .~ map toPlot dPlots
     $ polarlayout_margin .~ 10
     $ polarlayout_legend .~ mlegend
     $ polarlayout_axes .~ (polar_grid_style .~ (solidLine 1 (opaque orange)) 
                            $ polar_theta_axis_style .~ (solidLine 0 (opaque black))
                            $ polar_radial_axis_style .~ (solidLine 4 (withOpacity black 0.4))
                            $ polar_axes_label_style .~ (font_color .~ (withOpacity red 0.6) 
                                                         $ font_size .~ 18
                                                         $ def)
                            $ def)
     $ def


cfaChart :: 
  Bool
  -- ^ display the radial axis
  -> Bool
  -- ^ display the theta axis
  -> Bool
  -- ^ display grid lines
  -> Renderable ()
cfaChart r t g = toRenderable (cfaLayout r t g)

cfaBChart ::
  Bool
  -- ^ display the radial axis
  -> Bool
  -- ^ display the theta axis
  -> Bool
  -- ^ display grid lines
  -> Renderable ()
cfaBChart r t g = toRenderable (boundedLayout r t g)

cfaLayout :: 
  Bool
  -- ^ display the radial axis
  -> Bool
  -- ^ display the theta axis
  -> Bool
  -- ^ display grid lines
  -> PolarLayout Velocity RA
cfaLayout r t g = 
  let mkLine f = uncurry solidLine $ if f
                                     then (1, opaque black) 
                                     else (0, transparent)
      gLine = let (thick,col) = if g 
                                then (1, opaque orange)
                                else (0, transparent)
              in dashedLine thick [2,4] col
      bgCol = if r || t 
              then Nothing 
              else Just $ solidFillStyle (withOpacity green 0.1)
  in polarlayout_title .~ "CfA Redshift Survey: the \"Stick Man\""
     $ polarlayout_plots .~ [toPlot cfaPlot]
     $ polarlayout_plot_background .~ bgCol
     $ polarlayout_axes .~ (polar_radial_axis_style .~ mkLine r
                            $ polar_theta_axis_style .~ mkLine t
                            $ polar_grid_style .~ gLine
                            $ def)
     $ def
  
boundedLayout :: 
  Bool
  -- ^ display the radial axis
  -> Bool
  -- ^ display the theta axis
  -> Bool
  -- ^ display grid lines
  -> PolarLayout BVelocity RA
boundedLayout r t g = 
  let mkLine f = uncurry solidLine $ if f
                                     then (1, opaque black) 
                                     else (0, transparent)
      gLine = let (thick,col) = if g 
                                then (1, opaque orange)
                                else (0, transparent)
              in dashedLine thick [2,4] col
      bgCol = if r || t 
              then Nothing 
              else Just $ solidFillStyle (withOpacity green 0.1)
  in polarlayout_title .~ "CfA Redshift Survey: the \"Stick Man\""
     $ polarlayout_plots .~ [toPlot boundedPlot]
     $ polarlayout_plot_background .~ bgCol
     $ polarlayout_axes .~ (polar_radial_axis_style .~ mkLine r
                            $ polar_theta_axis_style .~ mkLine t
                            $ polar_grid_style .~ gLine
                            $ polar_axes_label_style .~ (font_size .~ 12
                                                         $ def)
                            $ def)
     $ def

cfaPlot :: PlotPoints Velocity RA
cfaPlot = 
  plot_points_style .~ pstyle
  $ plot_points_values .~ map swap cfa
  $ plot_points_title .~ "CfA"
  $ def
  where
    pstyle = point_shape .~ PointShapeCircle
             $ point_radius .~ 1
             $ point_color .~ opaque black
             $ point_border_color .~ opaque black
             $ point_border_width .~ 1
             $ def
          
cfaBounded :: [(BVelocity,RA)]
cfaBounded = 
  let f (a,Just b) = Just (b,a)
      f _          = Nothing
  in catMaybes $ map f $ map (second toBounded) cfa
             
boundedPlot :: PlotPoints BVelocity RA
boundedPlot = 
  plot_points_style .~ pstyle
  $ plot_points_values .~ cfaBounded
  $ plot_points_title .~ "CfA"
  $ def
  where
    pstyle = point_shape .~ PointShapeCircle
             $ point_radius .~ 4
             $ point_color .~ withOpacity orange 0.4
             $ point_border_color .~ opaque black
             $ point_border_width .~ 1
             $ def

-- | Data is taken from the CfA1 survey (not the June 2000 version) 
--   from <https://www.cfa.harvard.edu/~dfabricant/huchra/zcat/>.
--   It represents the RA and velocity/redshift of nearby galaxies,
--   and was important in shaping our understanding of how
--   large-scale structure in the Universe is arranged (the
--   \"stick man\" in the second figure shown on the page linked to
--   was rather a surprising discovery)
--
--   The `RA` and `Velocity` types are only intended for use    
--   in creating the CfA redshift plot, and not for general use.    
--
newtype RA = RA { unRA :: Double }
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)
newtype Velocity = V { unVelocity :: Double }
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)
newtype BVelocity = BV { unBVelocity :: Double }
  deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)

instance Bounded BVelocity where
  minBound = 5000
  maxBound = 12500

instance Show RA where
  show = show . unRA
  
instance Show Velocity where
  show = show . unVelocity
  
instance Show BVelocity where
  show = show . unBVelocity

instance PlotValue RA where
  toValue   = unRA
  fromValue = RA
  autoAxis  = autoScaledAxis def
  
instance PlotValue Velocity where
  toValue   = unVelocity
  fromValue = V
  autoAxis  = autoScaledAxis def

instance PolarPlotValue Velocity RA where
  autoPolarAxes = cfaAxes
                  
instance PlotValue BVelocity where
  toValue   = unBVelocity
  fromValue = BV
  autoAxis  = autoScaledAxis def

instance PolarPlotValue BVelocity RA where
  autoPolarAxes = boundedAxes
                  
-- convert a RA (in hours) into radians                  
ra2r :: RA -> Double                  
ra2r ra = pi * 15 * unRA ra / 180.0
  
toBounded :: Velocity -> Maybe BVelocity
toBounded (V v) = 
  let v1 = unBVelocity $ minBound
      v2 = unBVelocity $ maxBound
  in if v >= v1 && v <= v2
     then Just (BV v)
     else Nothing

skip1 :: [a] -> [a]          
skip1 (_:x:xs) = x : skip1 xs
skip1 _ = []
          
cfaAxes :: ([Velocity], [RA]) -> PolarAxesData Velocity RA
cfaAxes (rs,vs) = 
  let (rMin, rMax) = (0, 15000)
      (tMin, tMax) = (8, 17)
      
      -- try and remove "problem" positions/values
      f0 = filter (>0)
      g (xs, ys, zs) = (f0 xs, f0 ys, f0 zs)
      (rtickvs, rlabelvs, _) = g $ scaleLinear (5, 5) (0, rMax) rs
      rgridvs = skip1 rtickvs

      -- The angular tange is 8 to 17; so to get the mid-point of
      -- 12.5 at the top, need 0h to be drawn at 90+7.5 degrees
      -- (when measuring clockwise from the horizontal)
      tZero = pi/2 + (0.5*pi/12)
      thetaScale ra = tZero - ra2r ra
      radialScale radius r = unVelocity r * radius / unVelocity rMax
      
      rAngle = RA 8

      -- Is there any way to get a superscript?
      ttickvs = [9, 10, 11, 12, 13, 14, 15, 16]
      tlabelvs = map (\h -> (h, showD h ++ "h")) ttickvs
      tgridvs = skip1 ttickvs
      
      raxis = RadialAxisData {
        _radialaxis_visibility = def
        , _radialaxis_scale = radialScale
        , _radialaxis_range = (rMin, rMax)
        , _radialaxis_ticks = rtickvs
        , _radialaxis_labels = zip rtickvs $ map showD rlabelvs
        , _radialaxis_grid = rgridvs
        }        
              
      taxis = ThetaAxisData {
        _thetaaxis_visibility = def
        , _thetaaxis_scale = thetaScale
        , _thetaaxis_range = Just (tMin, tMax)
        , _thetaaxis_ticks = ttickvs 
        , _thetaaxis_labels = tlabelvs
        , _thetaaxis_grid = tgridvs
        }              
              
  in PolarAxesData {
    _polaraxes_r = raxis
    , _polaraxes_theta = taxis
    , _polaraxes_r_angle = rAngle
    }

{-
instance Default (PolarLayoutAxes Velocity RA) where
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
-}

boundedAxes :: ([BVelocity], [RA]) -> PolarAxesData BVelocity RA
boundedAxes (rs,vs) = 
  let (rMin, rMax) = (minBound, maxBound)
      (tMin, tMax) = (5, 20) -- use a different bound than the data
      
      -- try and remove "problem" positions/values
      f0 = filter (\r -> r>=rMin && r<=rMax)
      g (xs, ys, zs) = (f0 xs, f0 ys, f0 zs)
      (rtickvs, rlabelvs, _) = g $ scaleLinear (5, 5) (rMin, rMax) rs
      rgridvs = skip1 rtickvs

      -- The angular range is 8 to 17; so to get the mid-point of
      -- 12.5 at the top, need 0h to be drawn at 90+7.5 degrees
      -- (when measuring clockwise from the horizontal)
      tZero = pi/2 + (0.5*pi/12)
      thetaScale ra = tZero - ra2r ra
      radialScale radius r = unBVelocity r * radius / unBVelocity rMax
      
      rAngle = RA 8

      -- Is there any way to get a superscript?
      ttickvs = [9, 10, 11, 12, 13, 14, 15, 16]
      tlabelvs = map (\h -> (h, showD h ++ "h")) ttickvs
      tgridvs = skip1 ttickvs
      
      raxis = RadialAxisData {
        _radialaxis_visibility = def
        , _radialaxis_scale = radialScale
        , _radialaxis_range = (rMin, rMax)
        , _radialaxis_ticks = rtickvs
        , _radialaxis_labels = zip rtickvs $ map showD rlabelvs
        , _radialaxis_grid = rgridvs
        }        
              
      taxis = ThetaAxisData {
        _thetaaxis_visibility = def
        , _thetaaxis_scale = thetaScale
        , _thetaaxis_range = Just (tMin, tMax)
        , _thetaaxis_ticks = ttickvs 
        , _thetaaxis_labels = tlabelvs
        , _thetaaxis_grid = tgridvs
        }              
              
  in PolarAxesData {
    _polaraxes_r = raxis
    , _polaraxes_theta = taxis
    , _polaraxes_r_angle = rAngle
    }
  
-- data

cfa :: [(RA, Velocity)]
cfa = [(RA 8.86,V 3070),(RA 8.88,V 4032),(RA 8.89,V 3780),(RA 8.89,V 4865),(RA 8.89,V 7116),(RA 8.91,V 9197),(RA 8.92,V 2250),(RA 8.93,V 2538),(RA 8.94,V 1815),(RA 8.94,V 8393),(RA 8.95,V 9033),(RA 8.95,V 3085),(RA 8.95,V 3178),(RA 9.02,V 4744),(RA 9.03,V 2980),(RA 9.04,V 8128),(RA 9.04,V 7233),(RA 9.05,V 7025),(RA 9.06,V 11293),(RA 9.07,V 7158),(RA 9.07,V 553),(RA 9.08,V 7547),(RA 9.09,V 6964),(RA 9.09,V 3922),(RA 9.11,V 3956),(RA 9.11,V 1953),(RA 9.11,V 10328),(RA 9.11,V 4944),(RA 9.12,V 4854),(RA 9.12,V 5101),(RA 9.13,V 1363),(RA 9.13,V 2185),(RA 9.14,V 4257),(RA 9.15,V 2624),(RA 9.16,V 2016),(RA 9.16,V 3999),(RA 9.16,V 1951),(RA 9.16,V 7618),(RA 9.18,V 6745),(RA 9.18,V 2550),(RA 9.19,V 6909),(RA 9.19,V 4218),(RA 9.20,V 6313),(RA 9.21,V 600),(RA 9.23,V 1681),(RA 9.23,V 8424),(RA 9.23,V 1671),(RA 9.23,V 2322),(RA 9.24,V 1726),(RA 9.24,V 8299),(RA 9.24,V 1882),(RA 9.24,V 6538),(RA 9.25,V 8096),(RA 9.25,V 7623),(RA 9.27,V 2760),(RA 9.27,V 1736),(RA 9.28,V 6867),(RA 9.29,V 1694),(RA 9.30,V 1574),(RA 9.31,V 1486),(RA 9.31,V 631),(RA 9.32,V 7002),(RA 9.33,V 2691),(RA 9.33,V 1811),(RA 9.34,V 2732),(RA 9.35,V 2638),(RA 9.35,V 4879),(RA 9.36,V 1685),(RA 9.37,V 4099),(RA 9.39,V 2534),(RA 9.39,V 4317),(RA 9.40,V 3210),(RA 9.41,V 8030),(RA 9.42,V 7706),(RA 9.42,V 4215),(RA 9.43,V 1563),(RA 9.44,V 7541),(RA 9.46,V 1678),(RA 9.46,V 4294),(RA 9.47,V 4138),(RA 9.49,V 539),(RA 9.52,V 3217),(RA 9.52,V 3071),(RA 9.52,V 3171),(RA 9.54,V 2469),(RA 9.54,V 3748),(RA 9.55,V 6789),(RA 9.57,V 5995),(RA 9.57,V 8461),(RA 9.57,V 7556),(RA 9.57,V 4417),(RA 9.58,V 7549),(RA 9.58,V 4035),(RA 9.59,V 3344),(RA 9.60,V 8417),(RA 9.60,V 4421),(RA 9.61,V 4959),(RA 9.61,V 6497),(RA 9.62,V 760),(RA 9.63,V 3836),(RA 9.63,V 4841),(RA 9.63,V 6639),(RA 9.64,V 7013),(RA 9.64,V 8581),(RA 9.64,V 5809),(RA 9.65,V 1327),(RA 9.65,V 6732),(RA 9.67,V 1353),(RA 9.67,V 1465),(RA 9.67,V 8540),(RA 9.67,V 3322),(RA 9.67,V 3208),(RA 9.67,V 1544),(RA 9.68,V 4429),(RA 9.68,V 6200),(RA 9.70,V 5980),(RA 9.71,V 5489),(RA 9.72,V 3738),(RA 9.72,V 13),(RA 9.72,V 5020),(RA 9.73,V 3070),(RA 9.73,V 7222),(RA 9.74,V 7386),(RA 9.74,V 7439),(RA 9.76,V 4777),(RA 9.76,V 1478),(RA 9.78,V 1517),(RA 9.78,V 4666),(RA 9.79,V 8970),(RA 9.79,V 1440),(RA 9.80,V 1417),(RA 9.80,V 1545),(RA 9.80,V 1486),(RA 9.81,V 5200),(RA 9.81,V 561),(RA 9.82,V 1561),(RA 9.83,V 4790),(RA 9.83,V 5036),(RA 9.84,V 7595),(RA 9.84,V 1413),(RA 9.85,V 1292),(RA 9.85,V 3957),(RA 9.86,V 267),(RA 9.87,V 1444),(RA 9.87,V 1415),(RA 9.88,V 2996),(RA 9.88,V 1832),(RA 9.88,V 3731),(RA 9.89,V 9002),(RA 9.89,V 1236),(RA 9.89,V 3690),(RA 9.90,V 4593),(RA 9.91,V 3751),(RA 9.92,V 1381),(RA 9.92,V 5391),(RA 9.92,V 1484),(RA 9.93,V 1172),(RA 9.94,V 3566),(RA 9.95,V 10493),(RA 9.96,V 301),(RA 9.96,V 1154),(RA 9.96,V 2080),(RA 9.97,V 4185),(RA 9.98,V 1114),(RA 9.98,V 2409),(RA 9.99,V 7),(RA 9.99,V 1401),(RA 9.99,V 6989),(RA 10.01,V 3005),(RA 10.02,V 604),(RA 10.02,V 3066),(RA 10.02,V 6204),(RA 10.02,V 6139),(RA 10.03,V 2782),(RA 10.04,V 2895),(RA 10.04,V 3759),(RA 10.05,V 7394),(RA 10.07,V 8896),(RA 10.07,V 571),(RA 10.07,V 1314),(RA 10.08,V 552),(RA 10.08,V 2854),(RA 10.08,V 1121),(RA 10.08,V 1095),(RA 10.09,V 5199),(RA 10.09,V 2894),(RA 10.09,V 8206),(RA 10.10,V 290),(RA 10.10,V 5251),(RA 10.11,V 9212),(RA 10.13,V 7270),(RA 10.13,V 3830),(RA 10.15,V 3636),(RA 10.16,V 4757),(RA 10.16,V 8468),(RA 10.16,V 2268),(RA 10.17,V 1296),(RA 10.17,V 2811),(RA 10.17,V 6592),(RA 10.18,V 1303),(RA 10.18,V 6982),(RA 10.18,V 1317),(RA 10.19,V 1339),(RA 10.19,V 6195),(RA 10.19,V 1228),(RA 10.19,V 3315),(RA 10.20,V 1205),(RA 10.20,V 7783),(RA 10.23,V 1299),(RA 10.23,V 12601),(RA 10.25,V 1218),(RA 10.25,V 7258),(RA 10.25,V 1579),(RA 10.25,V 592),(RA 10.26,V 1293),(RA 10.26,V 1378),(RA 10.26,V 3780),(RA 10.27,V 9210),(RA 10.27,V 2130),(RA 10.28,V 660),(RA 10.29,V 2008),(RA 10.29,V 3296),(RA 10.29,V 6694),(RA 10.30,V 6197),(RA 10.30,V 7035),(RA 10.30,V 6992),(RA 10.30,V 1341),(RA 10.31,V 1161),(RA 10.31,V 1412),(RA 10.33,V 4085),(RA 10.33,V 5587),(RA 10.33,V 6824),(RA 10.33,V 6492),(RA 10.34,V 1226),(RA 10.35,V 1275),(RA 10.35,V 1138),(RA 10.36,V 2137),(RA 10.37,V 754),(RA 10.38,V 7079),(RA 10.39,V 1350),(RA 10.39,V 7369),(RA 10.39,V 5528),(RA 10.40,V 2150),(RA 10.41,V 1358),(RA 10.41,V 38),(RA 10.42,V 1450),(RA 10.43,V 9726),(RA 10.44,V 1366),(RA 10.44,V 5131),(RA 10.45,V 8071),(RA 10.47,V 1440),(RA 10.48,V 6259),(RA 10.48,V 8582),(RA 10.48,V 1703),(RA 10.49,V 929),(RA 10.49,V 1431),(RA 10.49,V 524),(RA 10.50,V 1650),(RA 10.50,V 1417),(RA 10.51,V 1136),(RA 10.52,V 7109),(RA 10.53,V 1516),(RA 10.53,V 3001),(RA 10.53,V 1422),(RA 10.54,V 1305),(RA 10.54,V 3338),(RA 10.54,V 7588),(RA 10.55,V 4413),(RA 10.56,V 1571),(RA 10.56,V 7713),(RA 10.56,V 2997),(RA 10.56,V 597),(RA 10.56,V 6755),(RA 10.57,V 2992),(RA 10.57,V 1343),(RA 10.57,V 6216),(RA 10.58,V 2887),(RA 10.58,V 1701),(RA 10.58,V 6896),(RA 10.59,V 989),(RA 10.60,V 743),(RA 10.61,V 854),(RA 10.61,V 2331),(RA 10.61,V 5607),(RA 10.61,V 1534),(RA 10.62,V 8136),(RA 10.62,V 5164),(RA 10.62,V 6288),(RA 10.63,V 5867),(RA 10.64,V 7202),(RA 10.65,V 10693),(RA 10.66,V 1299),(RA 10.67,V 1219),(RA 10.68,V 9045),(RA 10.68,V 582),(RA 10.68,V 1258),(RA 10.69,V 779),(RA 10.69,V 5744),(RA 10.69,V 6184),(RA 10.70,V 9809),(RA 10.70,V 8328),(RA 10.70,V 948),(RA 10.72,V 2032),(RA 10.72,V 1013),(RA 10.72,V 2831),(RA 10.73,V 986),(RA 10.73,V 3037),(RA 10.73,V 899),(RA 10.74,V 6287),(RA 10.74,V 1279),(RA 10.74,V 2713),(RA 10.75,V 8129),(RA 10.75,V 10612),(RA 10.75,V 5837),(RA 10.75,V 2731),(RA 10.75,V 689),(RA 10.75,V 922),(RA 10.75,V 6505),(RA 10.76,V 5928),(RA 10.76,V 1560),(RA 10.76,V 7818),(RA 10.76,V 728),(RA 10.76,V 1627),(RA 10.76,V 6265),(RA 10.76,V 4791),(RA 10.76,V 1261),(RA 10.77,V 2958),(RA 10.78,V 722),(RA 10.78,V 1625),(RA 10.79,V 1648),(RA 10.79,V 7230),(RA 10.79,V 3394),(RA 10.79,V 7144),(RA 10.80,V 1448),(RA 10.80,V 867),(RA 10.81,V 3158),(RA 10.81,V 2867),(RA 10.81,V 1181),(RA 10.81,V 1476),(RA 10.81,V 995),(RA 10.81,V 1280),(RA 10.81,V 685),(RA 10.81,V 3102),(RA 10.81,V 6471),(RA 10.81,V 6627),(RA 10.81,V 7953),(RA 10.81,V 3303),(RA 10.81,V 7541),(RA 10.81,V 1419),(RA 10.82,V 6109),(RA 10.82,V 9471),(RA 10.82,V 3641),(RA 10.82,V 2720),(RA 10.82,V 1614),(RA 10.83,V 611),(RA 10.83,V 6486),(RA 10.83,V 1114),(RA 10.83,V 1275),(RA 10.83,V 6534),(RA 10.84,V 1371),(RA 10.84,V 1244),(RA 10.84,V 1683),(RA 10.84,V 5793),(RA 10.85,V 1073),(RA 10.85,V 1066),(RA 10.85,V 1911),(RA 10.85,V 6577),(RA 10.86,V 1331),(RA 10.86,V 2026),(RA 10.86,V 5122),(RA 10.86,V 1357),(RA 10.86,V 1153),(RA 10.87,V 1102),(RA 10.87,V 1365),(RA 10.87,V 1156),(RA 10.88,V 6509),(RA 10.88,V 1898),(RA 10.90,V 9478),(RA 10.91,V 7537),(RA 10.93,V 6494),(RA 10.93,V 2607),(RA 10.93,V 2107),(RA 10.94,V 6667),(RA 10.95,V 7126),(RA 10.95,V 1861),(RA 10.95,V 2889),(RA 10.96,V 11310),(RA 10.96,V 6411),(RA 10.96,V 1436),(RA 10.96,V 681),(RA 10.96,V 693),(RA 10.97,V 6386),(RA 10.97,V 10602),(RA 10.97,V 2994),(RA 10.98,V 1145),(RA 10.98,V 8844),(RA 10.98,V 5974),(RA 10.99,V 7100),(RA 10.99,V 2936),(RA 10.99,V 6550),(RA 11.00,V 1037),(RA 11.00,V 1559),(RA 11.00,V 1133),(RA 11.00,V 7276),(RA 11.01,V 1511),(RA 11.01,V 6409),(RA 11.01,V 7554),(RA 11.01,V 980),(RA 11.02,V 704),(RA 11.02,V 7194),(RA 11.02,V 1370),(RA 11.03,V 8887),(RA 11.03,V 6479),(RA 11.03,V 7699),(RA 11.03,V 6303),(RA 11.04,V 7559),(RA 11.04,V 2073),(RA 11.05,V 804),(RA 11.06,V 2602),(RA 11.06,V 6412),(RA 11.06,V 1321),(RA 11.07,V 1254),(RA 11.07,V 2204),(RA 11.07,V 754),(RA 11.07,V 1318),(RA 11.08,V 6599),(RA 11.08,V 8801),(RA 11.09,V 1255),(RA 11.09,V 6171),(RA 11.10,V 1938),(RA 11.10,V 6962),(RA 11.11,V 968),(RA 11.11,V 7497),(RA 11.12,V 5987),(RA 11.12,V 12743),(RA 11.12,V 1543),(RA 11.13,V 9089),(RA 11.13,V 10521),(RA 11.13,V 2867),(RA 11.14,V 3233),(RA 11.14,V 7552),(RA 11.14,V 7580),(RA 11.14,V 698),(RA 11.14,V 6376),(RA 11.15,V 7447),(RA 11.16,V 6748),(RA 11.17,V 1402),(RA 11.18,V 5351),(RA 11.19,V 8763),(RA 11.19,V 3246),(RA 11.19,V 2130),(RA 11.20,V 693),(RA 11.20,V 6447),(RA 11.20,V 1969),(RA 11.21,V 8202),(RA 11.21,V 1193),(RA 11.21,V 6175),(RA 11.21,V 2248),(RA 11.21,V 850),(RA 11.21,V 7800),(RA 11.22,V 719),(RA 11.23,V 8956),(RA 11.24,V 686),(RA 11.24,V 973),(RA 11.24,V 951),(RA 11.24,V 1197),(RA 11.25,V 1620),(RA 11.25,V 2843),(RA 11.25,V 8082),(RA 11.26,V 6684),(RA 11.26,V 9857),(RA 11.26,V 2284),(RA 11.26,V 1125),(RA 11.26,V 2054),(RA 11.27,V 6805),(RA 11.27,V 801),(RA 11.28,V 1561),(RA 11.28,V 6245),(RA 11.28,V 6256),(RA 11.29,V 1328),(RA 11.29,V 1473),(RA 11.29,V 1966),(RA 11.29,V 724),(RA 11.29,V 843),(RA 11.29,V 1588),(RA 11.29,V 1514),(RA 11.30,V 2553),(RA 11.30,V 1520),(RA 11.30,V 1156),(RA 11.31,V 1302),(RA 11.31,V 1758),(RA 11.32,V 5449),(RA 11.32,V 4195),(RA 11.32,V 1587),(RA 11.33,V 2111),(RA 11.33,V 2096),(RA 11.34,V 1486),(RA 11.35,V 2828),(RA 11.35,V 1276),(RA 11.35,V 1215),(RA 11.35,V 1579),(RA 11.36,V 2044),(RA 11.36,V 1067),(RA 11.36,V 1370),(RA 11.37,V 2080),(RA 11.37,V 3725),(RA 11.38,V 3449),(RA 11.38,V 1940),(RA 11.39,V 1157),(RA 11.39,V 771),(RA 11.39,V 7399),(RA 11.39,V 7185),(RA 11.39,V 1885),(RA 11.40,V 646),(RA 11.40,V 1239),(RA 11.40,V 1210),(RA 11.41,V 5169),(RA 11.41,V 1162),(RA 11.41,V 1686),(RA 11.41,V 1515),(RA 11.42,V 1158),(RA 11.42,V 2501),(RA 11.43,V 2783),(RA 11.43,V 1071),(RA 11.43,V 3115),(RA 11.43,V 3101),(RA 11.43,V 6824),(RA 11.43,V 1709),(RA 11.43,V 7388),(RA 11.43,V 6311),(RA 11.44,V 2279),(RA 11.44,V 6261),(RA 11.44,V 6189),(RA 11.44,V 2429),(RA 11.45,V 2787),(RA 11.45,V 6277),(RA 11.46,V 1017),(RA 11.47,V 6491),(RA 11.48,V 6989),(RA 11.48,V 6636),(RA 11.48,V 1898),(RA 11.49,V 6984),(RA 11.49,V 2477),(RA 11.49,V 3648),(RA 11.49,V 5907),(RA 11.50,V 5979),(RA 11.50,V 987),(RA 11.50,V 3248),(RA 11.50,V 1851),(RA 11.51,V 1273),(RA 11.51,V 861),(RA 11.51,V 250),(RA 11.52,V 3284),(RA 11.52,V 6584),(RA 11.52,V 2619),(RA 11.52,V 1096),(RA 11.52,V 9438),(RA 11.53,V 2807),(RA 11.53,V 3212),(RA 11.54,V 8155),(RA 11.54,V 1188),(RA 11.55,V 5823),(RA 11.55,V 5949),(RA 11.55,V 225),(RA 11.55,V 2696),(RA 11.55,V 1598),(RA 11.56,V 211),(RA 11.56,V 1225),(RA 11.56,V 1565),(RA 11.57,V 1282),(RA 11.57,V 5577),(RA 11.57,V 5785),(RA 11.57,V 6237),(RA 11.57,V 1267),(RA 11.57,V 3950),(RA 11.58,V 3301),(RA 11.58,V 6362),(RA 11.58,V 3402),(RA 11.58,V 3283),(RA 11.59,V 724),(RA 11.59,V 3478),(RA 11.59,V 3214),(RA 11.59,V 1323),(RA 11.59,V 973),(RA 11.61,V 1154),(RA 11.61,V 2416),(RA 11.61,V 6229),(RA 11.61,V 740),(RA 11.62,V 2712),(RA 11.62,V 2627),(RA 11.62,V 10964),(RA 11.62,V 3434),(RA 11.62,V 1091),(RA 11.63,V 3325),(RA 11.63,V 3299),(RA 11.63,V 3527),(RA 11.63,V 3554),(RA 11.63,V 1850),(RA 11.63,V 3254),(RA 11.63,V 1821),(RA 11.63,V 1266),(RA 11.63,V 6472),(RA 11.64,V 7032),(RA 11.64,V 1385),(RA 11.64,V 993),(RA 11.64,V 3667),(RA 11.64,V 3443),(RA 11.64,V 3042),(RA 11.64,V 1468),(RA 11.65,V 3725),(RA 11.65,V 5548),(RA 11.65,V 786),(RA 11.66,V 6103),(RA 11.66,V 5749),(RA 11.66,V 6138),(RA 11.66,V 5504),(RA 11.66,V 3086),(RA 11.66,V 9051),(RA 11.66,V 6503),(RA 11.66,V 923),(RA 11.67,V 3132),(RA 11.68,V 6910),(RA 11.68,V 5928),(RA 11.69,V 6649),(RA 11.69,V 5908),(RA 11.69,V 5939),(RA 11.69,V 6280),(RA 11.69,V 2450),(RA 11.69,V 6237),(RA 11.69,V 1396),(RA 11.69,V 1300),(RA 11.70,V 2702),(RA 11.70,V 3350),(RA 11.70,V 6354),(RA 11.71,V 5082),(RA 11.71,V 6462),(RA 11.71,V 4572),(RA 11.71,V 6878),(RA 11.71,V 10617),(RA 11.71,V 2845),(RA 11.71,V 3090),(RA 11.71,V 1166),(RA 11.72,V 5438),(RA 11.72,V 3026),(RA 11.72,V 3210),(RA 11.72,V 750),(RA 11.72,V 903),(RA 11.73,V 6948),(RA 11.73,V 1431),(RA 11.74,V 7050),(RA 11.74,V 3103),(RA 11.74,V 5767),(RA 11.74,V 3574),(RA 11.75,V 2377),(RA 11.75,V 9253),(RA 11.76,V 5981),(RA 11.76,V 6202),(RA 11.77,V 977),(RA 11.77,V 3257),(RA 11.77,V 906),(RA 11.77,V 6398),(RA 11.77,V 3210),(RA 11.78,V 1799),(RA 11.78,V 1113),(RA 11.78,V 6815),(RA 11.78,V 3622),(RA 11.78,V 962),(RA 11.79,V 7840),(RA 11.79,V 1737),(RA 11.79,V 3622),(RA 11.79,V 1033),(RA 11.79,V 3757),(RA 11.80,V 6141),(RA 11.80,V 953),(RA 11.80,V 975),(RA 11.80,V 6244),(RA 11.81,V 5895),(RA 11.81,V 2355),(RA 11.81,V 3084),(RA 11.82,V 7130),(RA 11.82,V 919),(RA 11.82,V 974),(RA 11.82,V 3676),(RA 11.83,V 3067),(RA 11.84,V 6618),(RA 11.84,V 6500),(RA 11.84,V 6118),(RA 11.84,V 812),(RA 11.84,V 928),(RA 11.84,V 3638),(RA 11.84,V 1256),(RA 11.85,V 6225),(RA 11.85,V 6971),(RA 11.85,V 6456),(RA 11.85,V 786),(RA 11.85,V 1037),(RA 11.85,V 3212),(RA 11.86,V 1135),(RA 11.87,V 3322),(RA 11.87,V 3185),(RA 11.88,V 6973),(RA 11.88,V 6388),(RA 11.88,V 6795),(RA 11.88,V 5027),(RA 11.88,V 7108),(RA 11.88,V 1894),(RA 11.89,V 831),(RA 11.89,V 2496),(RA 11.89,V 9978),(RA 11.90,V 1188),(RA 11.90,V 909),(RA 11.90,V 946),(RA 11.90,V 3242),(RA 11.90,V 1065),(RA 11.90,V 3123),(RA 11.91,V 3369),(RA 11.91,V 776),(RA 11.91,V 1175),(RA 11.91,V 4500),(RA 11.91,V 3111),(RA 11.92,V 3118),(RA 11.92,V 1051),(RA 11.92,V 705),(RA 11.92,V 6989),(RA 11.92,V 3307),(RA 11.92,V 4771),(RA 11.92,V 1028),(RA 11.93,V 3366),(RA 11.93,V 4470),(RA 11.93,V 3680),(RA 11.93,V 835),(RA 11.93,V 3775),(RA 11.93,V 809),(RA 11.93,V 905),(RA 11.94,V 4341),(RA 11.94,V 3432),(RA 11.94,V 704),(RA 11.94,V 766),(RA 11.94,V 4390),(RA 11.94,V 1068),(RA 11.95,V 944),(RA 11.96,V 6144),(RA 11.97,V 1268),(RA 11.98,V 1505),(RA 11.98,V 936),(RA 11.98,V 1445),(RA 11.98,V 2384),(RA 11.99,V 1091),(RA 11.99,V 3086),(RA 11.99,V 1219),(RA 12.00,V 6462),(RA 12.00,V 3174),(RA 12.00,V 6131),(RA 12.00,V 1447),(RA 12.00,V 1942),(RA 12.01,V 4777),(RA 12.01,V 3421),(RA 12.01,V 839),(RA 12.01,V 710),(RA 12.02,V 4063),(RA 12.02,V 5800),(RA 12.02,V 7336),(RA 12.02,V 213),(RA 12.03,V 769),(RA 12.03,V 6374),(RA 12.03,V 2403),(RA 12.03,V 919),(RA 12.03,V 7386),(RA 12.03,V 7222),(RA 12.03,V 1275),(RA 12.03,V 5952),(RA 12.03,V 6220),(RA 12.03,V 1457),(RA 12.04,V 7028),(RA 12.04,V 7517),(RA 12.04,V 2529),(RA 12.04,V 621),(RA 12.05,V 750),(RA 12.05,V 4372),(RA 12.05,V 752),(RA 12.05,V 6741),(RA 12.06,V 7330),(RA 12.06,V 559),(RA 12.06,V 1080),(RA 12.06,V 862),(RA 12.07,V 10982),(RA 12.07,V 8220),(RA 12.07,V 2485),(RA 12.07,V 806),(RA 12.08,V 2575),(RA 12.08,V 1309),(RA 12.09,V 958),(RA 12.09,V 6767),(RA 12.09,V 1340),(RA 12.09,V 1674),(RA 12.09,V 1327),(RA 12.10,V 6330),(RA 12.10,V 2251),(RA 12.10,V 1813),(RA 12.10,V 2315),(RA 12.10,V 3731),(RA 12.10,V 1363),(RA 12.11,V 927),(RA 12.11,V 3828),(RA 12.11,V 6755),(RA 12.11,V 618),(RA 12.12,V 835),(RA 12.12,V 1141),(RA 12.12,V 966),(RA 12.12,V 7864),(RA 12.12,V 1980),(RA 12.12,V 285),(RA 12.13,V 1011),(RA 12.13,V 6019),(RA 12.13,V 6542),(RA 12.13,V 226),(RA 12.13,V 3056),(RA 12.13,V 999),(RA 12.13,V 2161),(RA 12.14,V 1761),(RA 12.14,V 6718),(RA 12.14,V 2670),(RA 12.14,V 2445),(RA 12.14,V 735),(RA 12.15,V 4941),(RA 12.15,V 2571),(RA 12.16,V 6954),(RA 12.16,V 165),(RA 12.16,V 3784),(RA 12.16,V 2307),(RA 12.16,V 1127),(RA 12.16,V 9274),(RA 12.16,V 3980),(RA 12.17,V 3956),(RA 12.17,V 2188),(RA 12.17,V 381),(RA 12.17,V 1228),(RA 12.18,V 2120),(RA 12.18,V 3904),(RA 12.18,V 934),(RA 12.18,V 9187),(RA 12.19,V 228),(RA 12.19,V 2053),(RA 12.19,V 2680),(RA 12.19,V 2480),(RA 12.20,V 2528),(RA 12.20,V 3975),(RA 12.20,V 2082),(RA 12.20,V 2376),(RA 12.21,V 1435),(RA 12.21,V 942),(RA 12.21,V 1117),(RA 12.21,V 861),(RA 12.21,V 701),(RA 12.21,V 2501),(RA 12.22,V 618),(RA 12.22,V 6576),(RA 12.22,V 6704),(RA 12.22,V 293),(RA 12.22,V 725),(RA 12.22,V 2067),(RA 12.22,V 135),(RA 12.22,V 1032),(RA 12.23,V 1274),(RA 12.23,V 954),(RA 12.23,V 2651),(RA 12.23,V 6450),(RA 12.23,V 7238),(RA 12.23,V 6813),(RA 12.24,V 7418),(RA 12.24,V 2771),(RA 12.24,V 2075),(RA 12.24,V 2371),(RA 12.24,V 2255),(RA 12.24,V 915),(RA 12.24,V 946),(RA 12.25,V 2235),(RA 12.25,V 403),(RA 12.25,V 247),(RA 12.25,V 516),(RA 12.25,V 815),(RA 12.25,V 2121),(RA 12.26,V 7299),(RA 12.26,V 3724),(RA 12.26,V 484),(RA 12.26,V 733),(RA 12.26,V 1067),(RA 12.26,V 7380),(RA 12.27,V 3876),(RA 12.27,V 2470),(RA 12.27,V 1696),(RA 12.27,V 2496),(RA 12.28,V 449),(RA 12.28,V 1524),(RA 12.28,V 3639),(RA 12.28,V 2492),(RA 12.28,V 1801),(RA 12.28,V 2200),(RA 12.28,V 2518),(RA 12.28,V 2633),(RA 12.28,V 1376),(RA 12.28,V 4108),(RA 12.29,V 2060),(RA 12.29,V 2535),(RA 12.29,V 1001),(RA 12.29,V 4753),(RA 12.29,V 2383),(RA 12.29,V 920),(RA 12.29,V 8453),(RA 12.29,V 2386),(RA 12.29,V 6670),(RA 12.29,V 2256),(RA 12.29,V 2628),(RA 12.29,V 643),(RA 12.30,V 2732),(RA 12.30,V 1076),(RA 12.30,V 6711),(RA 12.30,V 1715),(RA 12.30,V 532),(RA 12.31,V 3038),(RA 12.31,V 2258),(RA 12.31,V 359),(RA 12.31,V 867),(RA 12.31,V 6094),(RA 12.31,V 4227),(RA 12.32,V 1133),(RA 12.32,V 2310),(RA 12.32,V 221),(RA 12.32,V 6885),(RA 12.32,V 1163),(RA 12.32,V 1585),(RA 12.32,V 641),(RA 12.33,V 1934),(RA 12.33,V 1517),(RA 12.33,V 1193),(RA 12.33,V 1053),(RA 12.33,V 1685),(RA 12.33,V 1053),(RA 12.33,V 912),(RA 12.33,V 146),(RA 12.33,V 1004),(RA 12.34,V 1458),(RA 12.34,V 1262),(RA 12.34,V 1215),(RA 12.34,V 1560),(RA 12.34,V 2843),(RA 12.34,V 1681),(RA 12.34,V 4615),(RA 12.35,V 1552),(RA 12.35,V 1134),(RA 12.35,V 1298),(RA 12.35,V 762),(RA 12.35,V 1012),(RA 12.35,V 932),(RA 12.35,V 1146),(RA 12.35,V 751),(RA 12.36,V 1247),(RA 12.36,V 2106),(RA 12.36,V 2320),(RA 12.36,V 697),(RA 12.36,V 4191),(RA 12.36,V 4496),(RA 12.36,V 1173),(RA 12.36,V 1223),(RA 12.36,V 7019),(RA 12.37,V 1240),(RA 12.37,V 951),(RA 12.37,V 787),(RA 12.37,V 941),(RA 12.37,V 1649),(RA 12.38,V 9086),(RA 12.38,V 1033),(RA 12.38,V 1375),(RA 12.38,V 1071),(RA 12.38,V 1156),(RA 12.38,V 2545),(RA 12.38,V 2487),(RA 12.38,V 963),(RA 12.38,V 1694),(RA 12.38,V 758),(RA 12.38,V 1348),(RA 12.38,V 717),(RA 12.39,V 2142),(RA 12.39,V 1001),(RA 12.39,V 584),(RA 12.39,V 2535),(RA 12.39,V 1118),(RA 12.39,V 755),(RA 12.39,V 318),(RA 12.39,V 945),(RA 12.39,V 236),(RA 12.39,V 1742),(RA 12.40,V 7551),(RA 12.40,V 7600),(RA 12.40,V 1735),(RA 12.40,V 1280),(RA 12.40,V 96),(RA 12.40,V 720),(RA 12.40,V 910),(RA 12.40,V 1380),(RA 12.40,V 1269),(RA 12.40,V 843),(RA 12.41,V 1678),(RA 12.41,V 1599),(RA 12.41,V 6849),(RA 12.41,V 1092),(RA 12.41,V 432),(RA 12.41,V 1881),(RA 12.41,V 1472),(RA 12.41,V 1131),(RA 12.42,V 913),(RA 12.42,V 1068),(RA 12.42,V 2654),(RA 12.42,V 773),(RA 12.42,V 86),(RA 12.42,V 7110),(RA 12.42,V 739),(RA 12.43,V 515),(RA 12.43,V 300),(RA 12.43,V 669),(RA 12.43,V 203),(RA 12.43,V 1697),(RA 12.43,V 1957),(RA 12.44,V 860),(RA 12.44,V 223),(RA 12.44,V 1678),(RA 12.44,V 644),(RA 12.44,V 558),(RA 12.44,V 894),(RA 12.44,V 684),(RA 12.44,V 1215),(RA 12.44,V 1925),(RA 12.44,V 47469),(RA 12.45,V 1265),(RA 12.45,V 525),(RA 12.45,V 1600),(RA 12.45,V 498),(RA 12.45,V 895),(RA 12.45,V 2360),(RA 12.45,V 997),(RA 12.45,V 2236),(RA 12.46,V 1624),(RA 12.46,V 1955),(RA 12.46,V 1355),(RA 12.46,V 1840),(RA 12.46,V 1370),(RA 12.46,V 858),(RA 12.46,V 2422),(RA 12.47,V 1586),(RA 12.47,V 480),(RA 12.47,V 881),(RA 12.47,V 565),(RA 12.47,V 1292),(RA 12.47,V 1006),(RA 12.47,V 960),(RA 12.47,V 1802),(RA 12.47,V 497),(RA 12.47,V 450),(RA 12.48,V 4551),(RA 12.48,V 1350),(RA 12.48,V 3149),(RA 12.48,V 1098),(RA 12.48,V 4483),(RA 12.48,V 1795),(RA 12.48,V 1507),(RA 12.49,V 2100),(RA 12.49,V 2321),(RA 12.49,V 685),(RA 12.49,V 2732),(RA 12.49,V 1359),(RA 12.49,V 565),(RA 12.49,V 2270),(RA 12.50,V 1530),(RA 12.50,V 540),(RA 12.50,V 4698),(RA 12.50,V 8111),(RA 12.50,V 1131),(RA 12.51,V 503),(RA 12.51,V 940),(RA 12.51,V 958),(RA 12.51,V 2538),(RA 12.51,V 907),(RA 12.52,V 1229),(RA 12.52,V 2316),(RA 12.52,V 1226),(RA 12.52,V 1186),(RA 12.52,V 2326),(RA 12.53,V 602),(RA 12.53,V 1374),(RA 12.53,V 1736),(RA 12.53,V 803),(RA 12.53,V 8),(RA 12.53,V 2035),(RA 12.53,V 1962),(RA 12.53,V 1808),(RA 12.54,V 595),(RA 12.54,V 1287),(RA 12.54,V 2716),(RA 12.54,V 1286),(RA 12.54,V 6915),(RA 12.55,V 550),(RA 12.55,V 381),(RA 12.55,V 1126),(RA 12.55,V 6959),(RA 12.55,V 1198),(RA 12.55,V 322),(RA 12.55,V 6630),(RA 12.55,V 7434),(RA 12.56,V 1331),(RA 12.56,V 816),(RA 12.56,V 1407),(RA 12.56,V 5391),(RA 12.56,V 1227),(RA 12.57,V 1165),(RA 12.57,V 2186),(RA 12.57,V 7128),(RA 12.57,V 2232),(RA 12.57,V 1750),(RA 12.57,V 343),(RA 12.58,V 2284),(RA 12.59,V 1507),(RA 12.59,V 999),(RA 12.59,V 1985),(RA 12.59,V 1818),(RA 12.60,V 1737),(RA 12.60,V 819),(RA 12.60,V 901),(RA 12.61,V 4330),(RA 12.61,V 6944),(RA 12.61,V 2450),(RA 12.62,V 2839),(RA 12.62,V 1870),(RA 12.62,V 630),(RA 12.63,V 1961),(RA 12.63,V 787),(RA 12.63,V 150),(RA 12.63,V 1878),(RA 12.64,V 4732),(RA 12.64,V 1645),(RA 12.64,V 1737),(RA 12.64,V 1864),(RA 12.65,V 4639),(RA 12.65,V 1884),(RA 12.65,V 4801),(RA 12.65,V 4716),(RA 12.65,V 544),(RA 12.65,V 6829),(RA 12.66,V 609),(RA 12.66,V 444),(RA 12.66,V 1178),(RA 12.66,V 828),(RA 12.66,V 1788),(RA 12.66,V 620),(RA 12.67,V 1503),(RA 12.67,V 1712),(RA 12.67,V 685),(RA 12.67,V 304),(RA 12.67,V 144),(RA 12.67,V 960),(RA 12.67,V 937),(RA 12.67,V 1148),(RA 12.67,V 990),(RA 12.68,V 7492),(RA 12.68,V 4624),(RA 12.68,V 1319),(RA 12.68,V 1450),(RA 12.69,V 1095),(RA 12.69,V 800),(RA 12.69,V 1044),(RA 12.69,V 4942),(RA 12.69,V 646),(RA 12.70,V 521),(RA 12.70,V 1115),(RA 12.70,V 6991),(RA 12.71,V 6611),(RA 12.71,V 785),(RA 12.71,V 7023),(RA 12.71,V 1051),(RA 12.72,V 6938),(RA 12.74,V 5019),(RA 12.74,V 502),(RA 12.74,V 6760),(RA 12.75,V 1095),(RA 12.75,V 6739),(RA 12.75,V 4263),(RA 12.75,V 1616),(RA 12.75,V 1647),(RA 12.75,V 986),(RA 12.76,V 4928),(RA 12.76,V 8018),(RA 12.76,V 1185),(RA 12.77,V 1008),(RA 12.77,V 4062),(RA 12.78,V 727),(RA 12.79,V 1129),(RA 12.79,V 4379),(RA 12.79,V 655),(RA 12.79,V 7061),(RA 12.80,V 1206),(RA 12.80,V 1624),(RA 12.81,V 307),(RA 12.81,V 905),(RA 12.81,V 7531),(RA 12.81,V 8890),(RA 12.82,V 4867),(RA 12.82,V 1740),(RA 12.82,V 1190),(RA 12.82,V 1783),(RA 12.83,V 1374),(RA 12.83,V 8161),(RA 12.84,V 1244),(RA 12.84,V 1006),(RA 12.84,V 7036),(RA 12.85,V 777),(RA 12.85,V 1147),(RA 12.85,V 1042),(RA 12.86,V 2826),(RA 12.87,V 8354),(RA 12.87,V 2453),(RA 12.87,V 746),(RA 12.87,V 926),(RA 12.88,V 2812),(RA 12.88,V 2522),(RA 12.88,V 1330),(RA 12.88,V 2807),(RA 12.88,V 6941),(RA 12.89,V 2509),(RA 12.89,V 1552),(RA 12.89,V 762),(RA 12.90,V 6404),(RA 12.90,V 12518),(RA 12.90,V 7619),(RA 12.90,V 414),(RA 12.92,V 7335),(RA 12.92,V 6784),(RA 12.92,V 6266),(RA 12.93,V 1232),(RA 12.93,V 7227),(RA 12.93,V 5885),(RA 12.94,V 7652),(RA 12.95,V 847),(RA 12.95,V 4688),(RA 12.95,V 1980),(RA 12.95,V 7176),(RA 12.96,V 1470),(RA 12.96,V 6497),(RA 12.96,V 8446),(RA 12.97,V 962),(RA 12.97,V 1164),(RA 12.97,V 4663),(RA 12.98,V 7973),(RA 12.98,V 5475),(RA 12.99,V 7751),(RA 13.01,V 9249),(RA 13.02,V 6412),(RA 13.02,V 7111),(RA 13.04,V 5919),(RA 13.04,V 4750),(RA 13.05,V 6876),(RA 13.05,V 2525),(RA 13.06,V 2533),(RA 13.06,V 321),(RA 13.06,V 7150),(RA 13.06,V 4810),(RA 13.06,V 7135),(RA 13.07,V 8318),(RA 13.08,V 33),(RA 13.09,V 6499),(RA 13.10,V 8327),(RA 13.10,V 4794),(RA 13.11,V 5996),(RA 13.11,V 2460),(RA 13.11,V 8109),(RA 13.12,V 5488),(RA 13.12,V 5701),(RA 13.12,V 2858),(RA 13.12,V 8345),(RA 13.12,V 5615),(RA 13.14,V 3366),(RA 13.14,V 1022),(RA 13.14,V 7141),(RA 13.15,V 3811),(RA 13.15,V 5623),(RA 13.15,V 2619),(RA 13.15,V 1123),(RA 13.16,V 2613),(RA 13.16,V 815),(RA 13.16,V 8250),(RA 13.17,V 400),(RA 13.17,V 6400),(RA 13.17,V 3362),(RA 13.17,V 8748),(RA 13.18,V 8682),(RA 13.18,V 6325),(RA 13.19,V 875),(RA 13.20,V 7507),(RA 13.20,V 192),(RA 13.23,V 497),(RA 13.23,V 5600),(RA 13.24,V 6265),(RA 13.25,V 2794),(RA 13.25,V 5568),(RA 13.28,V 6630),(RA 13.29,V 2119),(RA 13.30,V 1283),(RA 13.31,V 5585),(RA 13.31,V 2122),(RA 13.32,V 940),(RA 13.32,V 3396),(RA 13.33,V 978),(RA 13.34,V 2583),(RA 13.34,V 2410),(RA 13.35,V 6976),(RA 13.35,V 8258),(RA 13.36,V 4826),(RA 13.36,V 6993),(RA 13.36,V 3146),(RA 13.36,V 6638),(RA 13.36,V 6908),(RA 13.37,V 7320),(RA 13.38,V 5303),(RA 13.38,V 5276),(RA 13.38,V 1225),(RA 13.40,V 1088),(RA 13.40,V 5606),(RA 13.41,V 7060),(RA 13.42,V 7322),(RA 13.42,V 6606),(RA 13.43,V 4647),(RA 13.44,V 2467),(RA 13.45,V 6840),(RA 13.45,V 4085),(RA 13.45,V 6712),(RA 13.45,V 8818),(RA 13.46,V 200),(RA 13.46,V 474),(RA 13.46,V 558),(RA 13.47,V 2569),(RA 13.47,V 7320),(RA 13.47,V 4135),(RA 13.47,V 1781),(RA 13.48,V 1000),(RA 13.49,V 1011),(RA 13.50,V 6761),(RA 13.50,V 9476),(RA 13.50,V 3272),(RA 13.51,V 6834),(RA 13.51,V 8792),(RA 13.51,V 2949),(RA 13.51,V 2860),(RA 13.51,V 8186),(RA 13.52,V 7313),(RA 13.52,V 4643),(RA 13.53,V 8103),(RA 13.54,V 7215),(RA 13.54,V 7062),(RA 13.54,V 7706),(RA 13.54,V 7041),(RA 13.54,V 6979),(RA 13.54,V 7365),(RA 13.54,V 243),(RA 13.55,V 3123),(RA 13.55,V 6855),(RA 13.56,V 2244),(RA 13.57,V 4525),(RA 13.57,V 4205),(RA 13.59,V 1156),(RA 13.59,V 7709),(RA 13.59,V 6023),(RA 13.60,V 6926),(RA 13.60,V 2364),(RA 13.60,V 8239),(RA 13.62,V 6799),(RA 13.62,V 6757),(RA 13.63,V 4848),(RA 13.64,V 5941),(RA 13.65,V 2035),(RA 13.66,V 7593),(RA 13.66,V 3134),(RA 13.66,V 7569),(RA 13.66,V 7603),(RA 13.67,V 1089),(RA 13.68,V 2203),(RA 13.68,V 10407),(RA 13.70,V 2502),(RA 13.72,V 2516),(RA 13.72,V 2583),(RA 13.74,V 2420),(RA 13.74,V 1503),(RA 13.74,V 5787),(RA 13.75,V 4894),(RA 13.76,V 2038),(RA 13.76,V 1396),(RA 13.76,V 1200),(RA 13.78,V 2693),(RA 13.78,V 2404),(RA 13.79,V 7023),(RA 13.79,V 1804),(RA 13.79,V 2537),(RA 13.80,V 2613),(RA 13.80,V 4307),(RA 13.81,V 1358),(RA 13.81,V 2625),(RA 13.81,V 10276),(RA 13.83,V 7109),(RA 13.83,V 6843),(RA 13.83,V 6721),(RA 13.83,V 4037),(RA 13.83,V 2208),(RA 13.83,V 6912),(RA 13.83,V 7548),(RA 13.83,V 2297),(RA 13.84,V 4397),(RA 13.84,V 2179),(RA 13.84,V 4690),(RA 13.84,V 3648),(RA 13.85,V 777),(RA 13.85,V 2335),(RA 13.85,V 2313),(RA 13.86,V 3630),(RA 13.86,V 2162),(RA 13.86,V 2635),(RA 13.86,V 7968),(RA 13.86,V 2408),(RA 13.86,V 1457),(RA 13.86,V 9129),(RA 13.87,V 13729),(RA 13.87,V 5545),(RA 13.87,V 1397),(RA 13.88,V 3061),(RA 13.88,V 4642),(RA 13.88,V 2225),(RA 13.88,V 141),(RA 13.88,V 1711),(RA 13.89,V 1092),(RA 13.89,V 2561),(RA 13.89,V 2080),(RA 13.89,V 6869),(RA 13.89,V 1256),(RA 13.90,V 1783),(RA 13.90,V 1752),(RA 13.91,V 1832),(RA 13.91,V 2383),(RA 13.91,V 6160),(RA 13.91,V 3042),(RA 13.91,V 3173),(RA 13.91,V 5529),(RA 13.92,V 4295),(RA 13.92,V 2133),(RA 13.92,V 2258),(RA 13.92,V 6833),(RA 13.93,V 5588),(RA 13.93,V 4312),(RA 13.93,V 5103),(RA 13.93,V 4296),(RA 13.94,V 3451),(RA 13.94,V 3493),(RA 13.94,V 9634),(RA 13.94,V 7667),(RA 13.97,V 5386),(RA 13.98,V 6922),(RA 13.98,V 5431),(RA 13.98,V 3764),(RA 13.98,V 2062),(RA 13.98,V 1869),(RA 13.99,V 2819),(RA 13.99,V 6259),(RA 13.99,V 4279),(RA 13.99,V 4817),(RA 14.00,V 6230),(RA 14.00,V 4544),(RA 14.01,V 5750),(RA 14.01,V 4278),(RA 14.01,V 6026),(RA 14.01,V 1801),(RA 14.01,V 3754),(RA 14.02,V 4634),(RA 14.02,V 2023),(RA 14.02,V 5733),(RA 14.02,V 3994),(RA 14.02,V 3901),(RA 14.02,V 233),(RA 14.03,V 5823),(RA 14.03,V 3829),(RA 14.04,V 7681),(RA 14.04,V 7147),(RA 14.04,V 5261),(RA 14.05,V 1997),(RA 14.06,V 1671),(RA 14.06,V 7235),(RA 14.06,V 399),(RA 14.07,V 1023),(RA 14.08,V 1860),(RA 14.08,V 2004),(RA 14.09,V 1989),(RA 14.10,V 1383),(RA 14.10,V 7100),(RA 14.10,V 5940),(RA 14.12,V 7733),(RA 14.12,V 1973),(RA 14.13,V 5008),(RA 14.13,V 5452),(RA 14.13,V 5116),(RA 14.14,V 2273),(RA 14.14,V 1891),(RA 14.14,V 5845),(RA 14.14,V 8424),(RA 14.16,V 5300),(RA 14.16,V 5217),(RA 14.17,V 4272),(RA 14.18,V 1872),(RA 14.18,V 7603),(RA 14.18,V 4993),(RA 14.19,V 7175),(RA 14.19,V 5909),(RA 14.20,V 4644),(RA 14.21,V 7570),(RA 14.21,V 2025),(RA 14.21,V 4595),(RA 14.21,V 4966),(RA 14.21,V 1040),(RA 14.21,V 5745),(RA 14.22,V 5555),(RA 14.22,V 2878),(RA 14.23,V 3864),(RA 14.24,V 5848),(RA 14.24,V 7367),(RA 14.24,V 7698),(RA 14.25,V 3040),(RA 14.25,V 3046),(RA 14.25,V 3132),(RA 14.26,V 7324),(RA 14.26,V 4419),(RA 14.26,V 5023),(RA 14.27,V 7404),(RA 14.27,V 7731),(RA 14.27,V 3258),(RA 14.29,V 5738),(RA 14.29,V 5628),(RA 14.29,V 1714),(RA 14.29,V 9139),(RA 14.30,V 1569),(RA 14.30,V 303),(RA 14.31,V 1582),(RA 14.31,V 7675),(RA 14.31,V 1542),(RA 14.31,V 1529),(RA 14.31,V 1435),(RA 14.31,V 7595),(RA 14.32,V 5007),(RA 14.32,V 3391),(RA 14.32,V 3242),(RA 14.33,V 2331),(RA 14.33,V 2240),(RA 14.34,V 7642),(RA 14.34,V 3167),(RA 14.34,V 5468),(RA 14.34,V 2220),(RA 14.35,V 10275),(RA 14.35,V 1392),(RA 14.35,V 5625),(RA 14.36,V 662),(RA 14.36,V 2349),(RA 14.36,V 3865),(RA 14.37,V 1957),(RA 14.37,V 3872),(RA 14.37,V 5087),(RA 14.38,V 153),(RA 14.39,V 4175),(RA 14.41,V 3890),(RA 14.41,V 8324),(RA 14.41,V 1929),(RA 14.42,V 3356),(RA 14.42,V 1950),(RA 14.43,V 2295),(RA 14.43,V 2658),(RA 14.43,V 1294),(RA 14.43,V 5626),(RA 14.44,V 4495),(RA 14.44,V 4325),(RA 14.44,V 5274),(RA 14.44,V 2895),(RA 14.45,V 9389),(RA 14.45,V 9014),(RA 14.45,V 4355),(RA 14.45,V 4331),(RA 14.45,V 1646),(RA 14.46,V 9077),(RA 14.47,V 7614),(RA 14.47,V 3514),(RA 14.47,V 2336),(RA 14.47,V 1363),(RA 14.47,V 5131),(RA 14.47,V 3192),(RA 14.47,V 5204),(RA 14.48,V 7496),(RA 14.48,V 3911),(RA 14.48,V 1431),(RA 14.48,V 2001),(RA 14.49,V 2359),(RA 14.50,V 2082),(RA 14.50,V 2266),(RA 14.51,V 1371),(RA 14.51,V 2234),(RA 14.51,V 3497),(RA 14.51,V 1929),(RA 14.51,V 3973),(RA 14.51,V 2221),(RA 14.51,V 2381),(RA 14.52,V 1581),(RA 14.52,V 2104),(RA 14.52,V 7564),(RA 14.55,V 7908),(RA 14.55,V 2200),(RA 14.56,V 2163),(RA 14.56,V 4082),(RA 14.57,V 2276),(RA 14.58,V 9430),(RA 14.58,V 5475),(RA 14.59,V 1750),(RA 14.59,V 3646),(RA 14.59,V 4262),(RA 14.60,V 1610),(RA 14.60,V 2208),(RA 14.61,V 2730),(RA 14.61,V 2239),(RA 14.61,V 5413),(RA 14.61,V 3700),(RA 14.61,V 1505),(RA 14.62,V 9060),(RA 14.64,V 2545),(RA 14.64,V 1601),(RA 14.64,V 3744),(RA 14.66,V 4695),(RA 14.66,V 3250),(RA 14.67,V 3744),(RA 14.67,V 1633),(RA 14.68,V 5579),(RA 14.70,V 1575),(RA 14.70,V 8893),(RA 14.70,V 3242),(RA 14.71,V 1724),(RA 14.72,V 4413),(RA 14.74,V 4013),(RA 14.74,V 8718),(RA 14.74,V 1692),(RA 14.76,V 5914),(RA 14.77,V 1788),(RA 14.82,V 1215),(RA 14.82,V 1306),(RA 14.83,V 4949),(RA 14.83,V 2126),(RA 14.84,V 2498),(RA 14.84,V 6936),(RA 14.84,V 1613),(RA 14.84,V 1571),(RA 14.85,V 1464),(RA 14.85,V 1589),(RA 14.86,V 1670),(RA 14.86,V 11346),(RA 14.86,V 8302),(RA 14.87,V 2330),(RA 14.87,V 5462),(RA 14.88,V 6070),(RA 14.89,V 5495),(RA 14.90,V 7263),(RA 14.90,V 4191),(RA 14.91,V 1807),(RA 14.91,V 4032),(RA 14.92,V 4835),(RA 14.92,V 8691),(RA 14.92,V 4097),(RA 14.93,V 1787),(RA 14.95,V 10003),(RA 14.95,V 3235),(RA 14.96,V 1369),(RA 14.96,V 448),(RA 14.98,V 1959),(RA 14.99,V 4030),(RA 14.99,V 626),(RA 15.00,V 6588),(RA 15.03,V 1683),(RA 15.03,V 10921),(RA 15.04,V 8611),(RA 15.05,V 1225),(RA 15.05,V 1359),(RA 15.05,V 8305),(RA 15.06,V 1456),(RA 15.07,V 1709),(RA 15.08,V 2527),(RA 15.08,V 5430),(RA 15.09,V 672),(RA 15.09,V 4772),(RA 15.09,V 1669),(RA 15.09,V 4764),(RA 15.09,V 6561),(RA 15.11,V 3128),(RA 15.12,V 1850),(RA 15.12,V 2470),(RA 15.12,V 2110),(RA 15.13,V 3527),(RA 15.13,V 3358),(RA 15.14,V 775),(RA 15.16,V 5630),(RA 15.18,V 2485),(RA 15.19,V 8738),(RA 15.20,V 5381),(RA 15.22,V 2554),(RA 15.23,V 6487),(RA 15.23,V 3391),(RA 15.24,V 669),(RA 15.26,V 3309),(RA 15.30,V 5169),(RA 15.32,V 1478),(RA 15.39,V 4458),(RA 15.41,V 2550),(RA 15.41,V 2617),(RA 15.41,V 6657),(RA 15.41,V 2619),(RA 15.44,V 5600),(RA 15.46,V 435),(RA 15.46,V 3986),(RA 15.46,V 5242),(RA 15.47,V 5521),(RA 15.48,V 10144),(RA 15.52,V 1782),(RA 15.52,V 6461),(RA 15.53,V 6514),(RA 15.54,V 1983),(RA 15.54,V 1955),(RA 15.54,V 652),(RA 15.54,V 1899),(RA 15.54,V 2022),(RA 15.55,V 5534),(RA 15.55,V 3416),(RA 15.55,V 1827),(RA 15.55,V 1766),(RA 15.57,V 4525),(RA 15.57,V 8857),(RA 15.57,V 5584),(RA 15.57,V 1963),(RA 15.57,V 5667),(RA 15.58,V 5669),(RA 15.58,V 5628),(RA 15.59,V 1447),(RA 15.59,V 5722),(RA 15.60,V 1960),(RA 15.61,V 6820),(RA 15.61,V 2611),(RA 15.62,V 2007),(RA 15.63,V 2936),(RA 15.63,V 4486),(RA 15.64,V 2517),(RA 15.65,V 2987),(RA 15.65,V 4092),(RA 15.68,V 2878),(RA 15.68,V 1105),(RA 15.71,V 9518),(RA 15.71,V 9578),(RA 15.73,V 2089),(RA 15.73,V 3809),(RA 15.75,V 3290),(RA 15.76,V 9964),(RA 15.79,V 4060),(RA 15.79,V 4458),(RA 15.80,V 3825),(RA 15.83,V 5958),(RA 15.83,V 12096),(RA 15.85,V 4865),(RA 15.85,V 834),(RA 15.85,V 10548),(RA 15.87,V 1846),(RA 15.87,V 2191),(RA 15.89,V 2429),(RA 15.90,V 5996),(RA 15.91,V 1791),(RA 15.92,V 10402),(RA 15.92,V 4397),(RA 15.92,V 4738),(RA 15.92,V 5931),(RA 15.93,V 744),(RA 15.93,V 4686),(RA 15.94,V 2169),(RA 15.94,V 9225),(RA 15.95,V 6011),(RA 15.97,V 4823),(RA 15.99,V 4491),(RA 16.01,V 9392),(RA 16.02,V 9264),(RA 16.03,V 3414),(RA 16.04,V 6035),(RA 16.05,V 4706),(RA 16.06,V 4583),(RA 16.07,V 1989),(RA 16.07,V 11755),(RA 16.07,V 11015),(RA 16.08,V 6605),(RA 16.13,V 4590),(RA 16.17,V 9245),(RA 16.17,V 8822),(RA 16.17,V 8876),(RA 16.18,V 10195),(RA 16.22,V 5794),(RA 16.23,V 9383),(RA 16.23,V 2928),(RA 16.24,V 8367),(RA 16.26,V 5615),(RA 16.28,V 3986),(RA 16.30,V 9203),(RA 16.30,V 4609),(RA 16.31,V 5066),(RA 16.33,V 9759),(RA 16.34,V 5054),(RA 16.34,V 5235),(RA 16.35,V 9306),(RA 16.39,V 8738),(RA 16.40,V 5982),(RA 16.41,V 2418),(RA 16.45,V 8446),(RA 16.45,V 9293),(RA 16.47,V 8800),(RA 16.51,V 5533),(RA 16.52,V 3355),(RA 16.52,V 10301),(RA 16.53,V 947),(RA 16.60,V 9424),(RA 16.60,V 6089),(RA 16.63,V 5213),(RA 16.69,V 852)]
