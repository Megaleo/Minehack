module Random where

import System.Random
import Test.QuickCheck

-- Convenient 'roll' dice \ random number generator
roll :: Int -> Int -> Int -> Int -> Int
roll min max determinant gen = round (toEnum(randomRs (min, (max * 100)) (mkStdGen gen) !! (determinant `mod` 100)) / 100)

-- Testing for longer it takes to a number repeate in a list 
repPeriod :: Int -> [Int] -> Int
repPeriod equal list = minimum . diffs . fst . unzip . filter equals $ zip [1..(length list)] list
  where 
    equals (_,a) = a == equal
    diffs (x:[]) = [999999]     
    diffs []     = [999999]
    diffs (x:xs) = [abs (head xs - x)] ++ diffs xs
    
checkPeriod :: Int -> IO()
checkPeriod min= quickCheck $ \y -> (repPeriod y $ map (\x -> roll 1 100 x 1615665) [1..1000]) > 50



