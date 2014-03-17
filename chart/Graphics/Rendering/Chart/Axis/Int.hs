-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Int
-- Copyright   :  (c) Tim Docker 2010, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render integer indexed axes

module Graphics.Rendering.Chart.Axis.Int(
    defaultIntAxis,
    scaledIntAxis,
    autoScaledIntAxis
) where

import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Axis.Floating
import Graphics.Rendering.Chart.Axis.Internal (stepsInt)

import Prelude hiding (min, max)

instance PlotValue Int where
    toValue    = fromIntegral
    fromValue  = round
    autoAxis   = autoScaledIntAxis defaultIntAxis

instance PlotValue Integer where
    toValue    = fromIntegral
    fromValue  = round
    autoAxis   = autoScaledIntAxis defaultIntAxis

-- | The default instance for `LinearAxisParams` is set up for
--   floating-point axes.
defaultIntAxis :: (Show a) => LinearAxisParams a
defaultIntAxis  = LinearAxisParams {
    _la_labelf  = show,
    _la_nLabels = 5,
    _la_nTicks  = 10
}

autoScaledIntAxis :: (Integral i, PlotValue i) =>
                     LinearAxisParams i -> AxisFn i
autoScaledIntAxis lap ps = scaledIntAxis lap rs ps
  where
    rs = (minimum ps,maximum ps)

scaledIntAxis :: (Integral i, PlotValue i) =>
                 LinearAxisParams i -> (i,i) -> AxisFn i
scaledIntAxis lap (minI,maxI) ps =
    makeAxis (_la_labelf lap) (labelvs,tickvs,gridvs)
  where
    range []  = (0,1)
    range _   | minI == maxI = (fromIntegral $ minI-1, fromIntegral $ minI+1)
              | otherwise    = (fromIntegral   minI,   fromIntegral   maxI)
--  labelvs  :: [i]
    labelvs   = stepsInt (fromIntegral $ _la_nLabels lap) r
    tickvs    = stepsInt (fromIntegral $ _la_nTicks lap)
                                  ( fromIntegral $ minimum labelvs
                                  , fromIntegral $ maximum labelvs )
    gridvs    = labelvs
    r         = range ps

