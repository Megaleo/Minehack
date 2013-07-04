module World.Perlin where

import Numeric.Noise
import Numeric.Noise.Perlin

-- Generates a 2D map with type Double from a Perlin 
twoD             :: Double -> Double -> Perlin -> Double
twoD x y perlin_ = noiseValue perlin_ (x,y,1)

-- Multiplies by 100 a 16x16 2D map of Perlin generated by "twoD" and rounds all the results
niceMap         :: Perlin -> [Int]
niceMap perlin_ = [round (100 * (twoD x y perlin_)) | x <- [1..16], y <- [1..16]]
