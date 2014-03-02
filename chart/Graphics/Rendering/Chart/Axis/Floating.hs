-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Floating
-- Copyright   :  (c) Tim Docker 2010
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Calculate and render floating value axes
-- including doubles with linear, log, and percentage scaling.
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Axis.Floating(
    Percent(..),
    LinearAxisParams(..),
    LogValue(..),
    LogAxisParams(..),
    defaultLinearAxis,
    defaultLogAxis,
    scaledAxis,
    autoScaledAxis,
    autoScaledLogAxis,
    autoSteps,

    la_labelf,
    la_nLabels,
    la_nTicks,

    loga_labelf
) where

import Data.Default.Class

import Prelude hiding (min, max)

import Control.Lens
import Graphics.Rendering.Chart.Utils (isValidNumber)
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Axis.Internal (logTicks, showD, steps)

instance PlotValue Double where
    toValue  = id
    fromValue= id
    autoAxis = autoScaledAxis defaultLinearAxis

-- | A wrapper class for doubles used to indicate they are to
-- be plotted against a percentage axis.
newtype Percent = Percent {unPercent :: Double}
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating,RealFloat)

instance Show Percent where
    show (Percent d) = showD (d*100) ++ "%"

instance PlotValue Percent where
    toValue  = unPercent
    fromValue= Percent
    autoAxis = autoScaledAxis defaultLinearAxis{-_la_labelf=-}

-- | A wrapper class for doubles used to indicate they are to
-- be plotted against a log axis.
newtype LogValue = LogValue Double
    deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Floating, RealFloat)

instance Show LogValue where
    show (LogValue x) = show x

instance PlotValue LogValue where
    toValue (LogValue x) = log x
    fromValue d          = LogValue (exp d)
    autoAxis             = autoScaledLogAxis defaultLogAxis

data LinearAxisParams a = LinearAxisParams {
    -- | The function used to show the axes labels.
    _la_labelf  :: a -> String,

    -- | The target number of labels to be shown.
    _la_nLabels :: Int,

    -- | The target number of ticks to be shown.
    _la_nTicks  :: Int
}

{-# DEPRECATED defaultLinearAxis "Use the according Data.Default instance!" #-}
defaultLinearAxis :: (Show a, RealFloat a) => LinearAxisParams a
defaultLinearAxis = def

instance (Show a, RealFloat a) => Default (LinearAxisParams a) where
  def = LinearAxisParams 
    { _la_labelf    = showD
    , _la_nLabels   = 5
    , _la_nTicks    = 50
    }

-- | Generate a linear axis with the specified bounds
scaledAxis :: RealFloat a => LinearAxisParams a -> (a,a) -> AxisFn a
scaledAxis lap (min,max) ps0 = makeAxis' realToFrac realToFrac
                                         (_la_labelf lap) (labelvs,tickvs,gridvs)
  where
    ps        = filter isValidNumber ps0
    range []  = (0,1)
    range _   | min == max = if min==0 then (-1,1) else
                             let d = abs (min * 0.01) in (min-d,max+d)
              | otherwise  = (min,max)
    labelvs   = map fromRational $ steps (fromIntegral (_la_nLabels lap)) r
    tickvs    = map fromRational $ steps (fromIntegral (_la_nTicks lap))
                                         (minimum labelvs,maximum labelvs)
    gridvs    = labelvs
    r         = range ps

-- | Generate a linear axis automatically, scaled appropriately for the
-- input data.
autoScaledAxis :: RealFloat a => LinearAxisParams a -> AxisFn a
autoScaledAxis lap ps0 = scaledAxis lap (min,max) ps0
  where
    (min,max) = (minimum ps0,maximum ps0)

-- | Given a target number of values, and a list of input points,
--   find evenly spaced values from the set {1*X, 2*X, 2.5*X, 5*X} (where
--   X is some power of ten) that evenly cover the input points.
autoSteps :: Int -> [Double] -> [Double]
autoSteps nSteps vs = map fromRational $ steps (fromIntegral nSteps) r
  where
    range []  = (0,1)
    range _   | min == max = (min-0.5,min+0.5)
              | otherwise  = (min,max)
    (min,max) = (minimum ps,maximum ps)
    ps        = filter isValidNumber vs
    r         = range ps

----------------------------------------------------------------------

{-# DEPRECATED defaultLogAxis "Use the according Data.Default instance!" #-}
defaultLogAxis :: (Show a, RealFloat a) => LogAxisParams a
defaultLogAxis = def

instance (Show a, RealFloat a) => Default (LogAxisParams a) where
  def = LogAxisParams 
    { _loga_labelf = showD
    }

-- | Generate a log axis automatically, scaled appropriate for the
-- input data.
autoScaledLogAxis :: RealFloat a => LogAxisParams a -> AxisFn a
autoScaledLogAxis lap ps0 =
    makeAxis' (realToFrac . log) (realToFrac . exp)
              (_loga_labelf lap) (wrap rlabelvs, wrap rtickvs, wrap rgridvs)
        where
          ps        = filter (\x -> isValidNumber x && 0 < x) ps0
          (min,max) = (minimum ps,maximum ps)
          wrap      = map fromRational
          range []  = (3,30)
          range _   | min == max = (realToFrac $ min/3, realToFrac $ max*3)
                    | otherwise  = (realToFrac $ min,   realToFrac $ max)
          (rlabelvs, rtickvs, rgridvs) = logTicks (range ps)


data LogAxisParams a = LogAxisParams {
    -- | The function used to show the axes labels.
    _loga_labelf :: a -> String
}

$( makeLenses ''LinearAxisParams )
$( makeLenses ''LogAxisParams )

