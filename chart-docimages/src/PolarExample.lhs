
This should match the example used in Graphics/Rendering/Chart/PolarLayout.hs

This is done as a literate file to make it easier to cut and paste
into the module

Switching to diagrams for now, as easier to build this back end.

import qualified Graphics.Rendering.Chart.Backend.Cairo as C

> import qualified Data.Map.Lazy as M
> import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
>
> import Control.Applicative ((<$>))
> import Control.Lens
> import Control.Monad (replicateM, void)
> import Data.Colour
> import Data.Colour.Names
> import Data.Default.Class
> import Data.List (zip5)
> import Graphics.Rendering.Chart
> import Graphics.Rendering.Chart.Utils (LUT, fromLUT, cubeHelix0)
> import System.Random
>
> -- randomly chose r and theta values; the circle radius scales with
> -- r, and the color maps to theta via the LUT.
> makeData :: LUT (Colour Double) -> IO [PlotPoints Double Double]
> makeData lut = do
>   let npts = 150
>       rand = replicateM npts (randomIO :: IO Double)
>       shapes = [ PointShapeCircle
>                , PointShapeCircle
>                , PointShapeCircle
>                , PointShapeCircle
>                , PointShapeCircle
>                , PointShapeCircle
>                , PointShapePolygon 3 True
>                , PointShapePolygon 4 True
>                , PointShapePolygon 5 True
>                , PointShapePolygon 6 True
>                , PointShapePlus
>                , PointShapeCross
>                , PointShapeStar
>                ]
>   r1 <- rand
>   r2 <- rand
>   r3 <- rand
>   shps <- replicateM npts (randomRIO (0,length shapes -1))
>   let rs = map (2*) r1
>       rads = zipWith (\ra rb -> sqrt (ra*100) * rb) rs r3
>       cols = map (flip withOpacity 0.4 . fromLUT lut) r2
>       thetas = map (2*pi*) r2
>       pitem (r,t,s,c,shp) = plot_points_style .~ pstyle s c shp
>                           $ plot_points_values .~ [(r,t)]
>                           $ def
>       pstyle s c shp = point_shape .~ shapes !! shp
>                      $ point_color .~ c
>                      $ point_radius .~ s
>                      $ point_border_color .~ opaque black
>                      $ point_border_width .~ 1
>                      $ def
>                    
>   return $ map pitem $ zip5 rs thetas rads cols shps
> 
> lPlot :: PlotLines Double Double
> lPlot =
>   let lvs = map (\r -> (r, 2*pi*r)) [0, 0.01 .. 2]
>   in plot_lines_values .~ [lvs]
>      $ plot_lines_style . line_color .~ withOpacity red 0.4
>      $ plot_lines_style . line_width .~ 2
>      $ def
> 
> bgFill, pFill :: FillStyle
> bgFill = solidFillStyle (opaque gray)
> pFill = solidFillStyle (withOpacity orange 0.4)
>       
> testPlot :: ToPlot p => [p Double Double] -> PolarLayout Double Double
> testPlot pps = 
>   polarlayout_title .~ "Polar plot"
>     $ polarlayout_background .~ bgFill
>     $ polarlayout_plot_background .~ Just pFill
>     $ polarlayout_plots .~ toPlot lPlot : map toPlot pps
>     $ polarlayout_margin .~ 10
>     $ def
> 
> makePlot :: IO ()
> makePlot = do
>   setStdGen $ mkStdGen 49 -- "repeatable" randomness ;-)
>   r <- toRenderable . testPlot <$> makeData cubeHelix0
>   let fopts = D.FileOptions (400,400) D.SVG M.empty
>   void $ D.renderableToFile fopts r "polar-example1.svg"
>
> main :: IO ()
> main = makePlot

   void $ C.renderableToFile (C.FileOptions (400,400) C.SVG) r "polar-example1.svg"
