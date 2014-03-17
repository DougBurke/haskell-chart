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
  , scaleLinear
) where

import Data.List (genericLength, minimumBy)
import Data.Ord (comparing)

import Numeric (showFFloat)

import Graphics.Rendering.Chart.Geometry (Range)
import Graphics.Rendering.Chart.Utils (isValidNumber, log10)

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
  pf :: RealFrac a => a -> (Integer, a)
  pf = properFraction

  -- frac :: (RealFrac a, Integral b) => a -> (b, a)
  frac :: (RealFrac a) => a -> (Integer, a)
  frac x | 0 <= b    = (a,b)
         | otherwise = (a-1,b+1)
    where
      (a,b) = properFraction x

  ratio      = high/low
  lower a l  = let (i,r) = frac (log10 a) in
               maximum (1:filter (\x -> log10 (fromRational x) <= r) l)*10^^i
  upper a l  = let (i,r) = pf (log10 a) in
               minimum (10:filter (\x -> r <= log10 (fromRational x)) l)*10^^i
               
  powers           :: (Double,Double) -> [Rational] -> [Rational]
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
  ratio' = fromRational (h'/l')
  filterX = filter (\x -> l'<=x && x <=h') . powers (dl',dh') 
  
  minor | 50 < log10 ratio' = map roundPow $
                              steps 50 (log10 dl', log10 dh')
        | 6 < log10 ratio'  = filterX [1,10]
        | 3 < log10 ratio'  = filterX [1,5,10]
        | 6 < ratio'        = filterX [1..10]
        | 3 < ratio'        = filterX [1,1.2..10]
        | otherwise         = steps 50 (dl', dh')

{- 
Create grid values for a linear axis, based on scaledAxis; the code
in scaledAxis should probably be refactored to use these.
-}

-- Select a suitable range for the axis, given the points to plot,
-- which can be empty.
linearRange :: 
  (Eq a, Fractional a) =>
  (a, a)    -- ^ bounds of data (used when the data contains at least 1 valid element)   
  -> [a]    -- ^ data (only use is to check if null or not)
  -> (a, a) -- ^ bounds to use
linearRange _ [] = (0,1)
linearRange r@(minval,maxval) _ 
  | minval == maxval = if minval == 0 then (-1,1) else 
                         let d = abs (minval * 0.01) in (minval-d, maxval+d)
  | otherwise  = r

-- why bother sending in both the bounds and the data in; would it not make sense
-- to have the caller perform the linearRange logic first?
scaleLinear :: 
  RealFloat a =>
  (Int, Int)          -- ^ num labels, num ticks
  -> (a, a)           -- ^ min,max bounds
  -> [a]              -- ^ data values
  -> ([a], [a], [a])  -- ^ tick, label, grid positions 
scaleLinear (nLabels,nTicks) rng ps0 = 
  let ps = filter isValidNumber ps0
      lr = linearRange rng ps
      tr = (minimum labelvs, maximum labelvs)
      
      labelvs = map fromRational $ steps (fromIntegral nLabels) lr 
      tickvs  = map fromRational $ steps (fromIntegral nTicks) tr
      gridvs  = labelvs
      
  in (labelvs, gridvs, tickvs)

