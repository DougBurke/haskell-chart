-- | Non chart specific utility functions.
module Graphics.Rendering.Chart.Utils(
    isValidNumber,
    maybeM,
    
    maximum0,
    log10,
    
    -- * Look-Up Tables
    LUT,
    fromLUT,
    toLUT,
    cubeHelix,
    cubeHelix0
  ) where

import Data.Colour.SRGB (Colour, sRGB)
import Data.List (zip4)
import Data.Maybe (fromJust)

-- | Checks if the given value is and actual numeric value and not 
--   a concept like NaN or infinity.
isValidNumber :: (RealFloat a) => a -> Bool
isValidNumber v = not (isNaN v) && not (isInfinite v)

-- | Version of 'Prelude.maybe' that returns a monadic value.
maybeM :: (Monad m) => b -> (a -> m b) -> Maybe a -> m b
maybeM v = maybe (return v)

-- | Return the maximum value in an array if non-empty,
--   otherwise 0.
maximum0 :: (Num a, Ord a) => [a] -> a
maximum0 [] = 0
maximum0 vs = maximum vs

-- | Return the log of the input, in base 10.
log10 :: (Floating a) => a -> a
log10 = logBase 10
  
-- | A simple (in other words /experimental/) representation of a look-up table.
--   There must be something like this in one of the container-like libraries
--   somewhere, but I am too lazy to look.
--
--   It has not been optimized (for speed or size), or looked at to see if it
--   is sufficiently general (e.g. can it be used on an enumerated, ordered,
--   and bounded type as the \"index\" type in `fromLUT`?).

-- QUS: are the bang patterns doing anything here?
data LUT a = 
  LUT
  { _lutSize :: !Int    -- number of entries
  , _lutElems :: [a]    -- list (finite, non-empty); a Vector would be better
  , _lutFirst :: !a     -- first element
  , _lutLast  :: !a     -- last element
  }

-- | Create a look-up table.
toLUT :: 
  [a] -- ^ items to add to the table; must not be empty.
  -> Maybe (LUT a)
toLUT [] = Nothing
toLUT xs = Just LUT { 
  _lutSize = length xs
  , _lutElems = xs
  , _lutFirst = head xs
  , _lutLast = last xs
  }
           
-- | Extract a value from a look-up table.
fromLUT :: 
  RealFrac i =>
  LUT a
  -> i 
  -- ^ index value, in the range @[0,1)@ (that is 0 is included in the range but 1 is not).
  --   Values less than 0 are mapped to the first item; if 1 or greater then the last item.
  -> a
fromLUT l f =
  let n = _lutSize l
      e0 = _lutFirst l
      e1 = _lutLast l
      -- if I used truncate here than f=-0.1 would map to i=0; this doesn't
      -- really matter as now not returning Maybe a, but left in
      i = floor $ f * fromIntegral n
  in if i <= 0 then e0 else if i >= (n-1) then e1 else _lutElems l !! i
  
-- | Create a LUT following the cube helix scheme of
--   D.A. Green, 'A colour scheme for the display of astronomical intensity images'
--   <http://adsabs.harvard.edu/abs/2011BASI...39..289G>
--     
--   At present 
cubeHelix ::
  Double     -- ^ start
  -> Double  -- ^ number of rotations
  -> Double  -- ^ hue
  -> Double  -- ^ gamma
  -> Int     -- ^ number of entries, must be more than 0 otherwise `Nothing` is returned
  -> Bool    -- ^ should the r,g,b values be clipped?
  -> Maybe (LUT (Colour Double))
cubeHelix start nrots hue gamma nlev clip =   
  let norm x = fromIntegral x / fromIntegral (nlev - 1)
      fract = map norm [0 .. (nlev-1)]
      gfract = map (**gamma) fract
      
      toAngle f = 2 * pi * (start/3.0 + 1 + nrots * f)
      angle = map toAngle fract
      
      toAmp gf = hue * gf * (1-gf) / 2
      amp = map toAmp gfract
      
      cosa = map cos angle
      sina = map sin angle
      
      toR (f,a,ca,sa) = f + a * (-0.14861 * ca + 1.78277 * sa)
      toG (f,a,ca,sa) = f + a * (-0.29227 * ca -0.90649 * sa)
      toB (f,a,ca,_) = f + a * 1.97294 * ca

      ls = zip4 fract amp cosa sina
      rs = map toR ls
      gs = map toG ls
      bs = map toB ls
      
      toClip x | x < 0 = 0
               | x > 1 = 1
               | otherwise = x
                             
      conv = if clip then toClip else id
      crs = map conv rs
      cgs = map conv gs
      cbs = map conv bs
      
  in toLUT $ zipWith3 sRGB crs cgs cbs

-- | Create a `cubeHelix` look-up table using the default/suggested
--   parameter values:
--
--    - start = 0.5
--    - nrots = -1.5
--    - hue = 1.0
--    - gamma = 1.0
--    - nlevels = 256
--    - clip=True
--       
cubeHelix0 :: LUT (Colour Double)
cubeHelix0 = fromJust $ cubeHelix 0.5 (-1.5) 1.0 1.0 256 True
