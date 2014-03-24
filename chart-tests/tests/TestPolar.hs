-- | Test out Graphics.Rendering.Chart.Polar
-- 
module TestPolar where 

import Control.Arrow (second)
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart

import Utils

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



