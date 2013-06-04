module Random where

import System.Random

-- Convenient 'roll' dice \ random number generator
roll :: Int -> Int -> Int -> Int -> Int
roll min max determinant gen = round (toEnum(randomRs (min, (max * 100)) (mkStdGen gen) !! (determinant `mod` 100)) / 100)


