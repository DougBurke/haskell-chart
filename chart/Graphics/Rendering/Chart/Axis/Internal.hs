-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Axis.Internal
-- Copyright   :  (c) Tim Docker 2010, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Allow potentially-useful routines to be shared between different
-- implementations.
--

module Graphics.Rendering.Chart.Axis.Internal(
  showD
  , steps
  , stepsInt
  , chooseStep
  , logTicks
) where

{-
import Data.Default.Class

import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Utils
import Graphics.Rendering.Chart.Axis.Types
-}

import Data.List (genericLength, minimumBy)
import Data.Ord (comparing)

import Numeric (showFFloat)

import Graphics.Rendering.Chart.Geometry (Range)
import Graphics.Rendering.Chart.Utils (log10)

-- using minV,maxV (for minVal,maxVal) to avoid shadowing min and max

steps :: RealFloat a => a -> (a,a) -> [Rational]
steps nSteps (minV,maxV) = map ((s*) . fromIntegral) [min' .. max']
  where
    s    = chooseStep nSteps (minV,maxV)
    min' :: Integer
    min' = floor   $ realToFrac minV / s
    max' = ceiling $ realToFrac maxV / s
    -- n    = (max' - min')

chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  = 10 ^^ ((floor $ log10 $ delta / nsteps) :: Integer)
    stepVals = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps

stepsInt :: Integral a => a -> Range -> [a]
stepsInt nSteps range = bestSize (goodness alt0) alt0 alts
  where
    bestSize _ _ [] = error "empty list in Graphics.Rendering.Chart.Axis.Internal.stepsInt" -- quiet -Wall
    bestSize n a (a':as) = let n' = goodness a' in
                           if n' < n then bestSize n' a' as else a

    goodness vs          = abs (genericLength vs - nSteps)

    (alt0:alts)          = map (\n -> stepsInt' n range) sampleSteps

    sampleSteps          = [1,2,5] ++ sampleSteps1
    sampleSteps1         = [10,20,25,50] ++ map (*10) sampleSteps1

-- not sure if this needs to be a top-level routine, but it shares similarities
-- to steps.
--
stepsInt' :: 
  (Integral a, RealFrac b) =>
  a 
  -> (b, b) -- technically these can be different RealFrac instances but simplify here
  -> [a]
stepsInt' size (minV,maxV) = takeWhile (<b) [a,a+size..] ++ [b]
  where
    a = floor   (minV / fromIntegral size) * size
    b = ceiling (maxV / fromIntegral size) * size

-- | Convert a number to a string, dropping the @\".0\"@ suffix
--   if it exists.
showD :: (RealFloat d) => d -> String
showD x = case reverse $ showFFloat Nothing x "" of
            '0':'.':r -> reverse r
            r         -> reverse r

{-
 Rules: Do not subdivide between powers of 10 until all powers of 10
          get a major ticks.
        Do not subdivide between powers of ten as [1,2,4,6,8,10] when
          5 gets a major ticks
          (ie the major ticks need to be a subset of the minor tick)
-}
logTicks :: Range -> ([Rational],[Rational],[Rational])
logTicks (low,high) = (major,minor,major)
 where
  ratio      = high/low
  lower a l  = let (i,r) = frac (log10 a) in
               maximum (1:filter (\x -> log10 (fromRational x) <= r) l) * 10^^i
  upper a l  = let (i,r) = pf (log10 a) in
               minimum (10:filter (\x -> r <= log10 (fromRational x)) l) *10^^i
  
  powers :: (Double,Double) -> [Rational] -> [Rational]
  powers (x,y) l    = [ a*10^^p | p <- [(floor (log10 x))..(ceiling (log10 y))] :: [Integer]
                                , a <- l ]
                      
  midselection r l  = filter (inRange r l) (powers r l)
  inRange (a,b) l x = (lower a l <= x) && (x <= upper b l)
  
  logRange = (log10 low, log10 high)
  
  roundPow x = 10^^(round x :: Integer)
  
  major | 17.5 < log10 ratio = map roundPow $
                               steps (min 5 (log10 ratio)) logRange
        | 12 < log10 ratio   = map roundPow $
                               steps (log10 ratio / 5) logRange
        | 6 < log10 ratio    = map roundPow $
                               steps (log10 ratio / 2) logRange
        | 3 < log10 ratio    = midselection (low,high) [1,10]
        | 20 < ratio         = midselection (low,high) [1,5,10]
        | 6 < ratio          = midselection (low,high) [1,2,4,6,8,10]
        | 3 < ratio          = midselection (low,high) [1..10]
        | otherwise          = steps 5 (low,high)
  
  (l',h')   = (minimum major, maximum major)
  (dl',dh') = (fromRational l', fromRational h')
  ratio' :: Double
  ratio'    = fromRational (h'/l')
  filterX = filter (\x -> l'<=x && x <=h') . powers (dl',dh') 
  
  minor | 50 < log10 ratio' = map roundPow $
                              steps 50 (log10 dl', log10 dh')
        | 6 < log10 ratio'  = filterX [1,10]
        | 3 < log10 ratio'  = filterX [1,5,10]
        | 6 < ratio'        = filterX [1..10]
        | 3 < ratio'        = filterX [1,1.2..10]
        | otherwise         = steps 50 (dl', dh')

  -- frac :: (Integral b, RealFrac a) => a -> (b, a)
  -- frac :: RealFrac a => a -> (Integer, a)
  frac x | 0 <= b    = (a,b)
         | otherwise = (a-1,b+1)
    where
      (a,b) = pf x

  -- force the types to avoid warnings about defaulting from -Wall
  -- pf :: (Integral b, RealFrac a) => a -> (b, a)
  pf :: RealFrac a => a -> (Integer, a)
  pf = properFraction
  