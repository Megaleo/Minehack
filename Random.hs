module Random where

-- #@TODO@#
--
-- @Inplement support for other "Num" types in "Roll"

import System.Random
import Test.QuickCheck

-- Convenient 'roll' dice \ random number generator
rollSeed :: Int -> Int -> Int -> Int -> Int
rollSeed min max determinant seed = round (toEnum(randomRs (min, (max * 100)) (mkStdGen seed) !! (determinant `mod` 100)) / 100)

-- Roll min max seed or Fix number
data Roll where
  Roll :: Int -> Int -> Int -> Roll 
  Fix  :: Int -> Roll 

roll :: Roll -> Int
roll (Roll min max seed) = rollSeed min max (seed `mod` 200) seed

-- Testing for how far it takes to a number repeat in a list 
repPeriod :: Int -> [Int] -> Int
repPeriod equal list = minimum . diffs . fst . unzip . filter equals $ zip [1..(length list)] list
  where 
    equals (_,a) = a == equal
    diffs (x:[]) = [999999]     
    diffs []     = [999999]
    diffs (x:xs) = [abs (head xs - x)] ++ diffs xs
    
checkPeriod :: Int -> IO()
checkPeriod min= quickCheck $ \y -> (repPeriod y $ map (\x -> rollSeed 1 100 x 1615665) [1..1000]) > 50



