module Random where

import System.Random
import Test.QuickCheck

-- | Convenient 'roll' dice \ random number generator.
rollSeed :: Int -> Int -> Int -> Int -> Int
rollSeed min_ max_ determinant seed = round ((toEnum(randomRs (min_, (max_ * 100)) (mkStdGen seed) !! (determinant `mod` 100)) / 100) :: Double)

-- | Roll min_ max_ or Fix number.
data Roll = Roll Int Int
          | Fix Int

-- | Takes a Roll type and a Seed to generate a Integer.
roll :: Roll -> Int -> Int
roll (Roll min_ max_) seed = rollSeed min_ max_ (seed `mod` 200) seed
roll (Fix value) _         = value

-- | Testing for how far it takes to a number repeat in a list.
repPeriod :: Int -> [Int] -> Int
repPeriod equal list = minimum . diffs . fst . unzip . filter equals $ zip [1..(length list)] list
  where
    equals (_,a) = a == equal
    diffs (_:[]) = [999999]
    diffs []     = [999999]
    diffs (x:xs) = [abs (head xs - x)] ++ diffs xs

-- | Checks the period of repetition using repPeriod.
checkPeriod :: IO()
checkPeriod = quickCheck $ \y -> (repPeriod y $ map (\x -> rollSeed 1 100 x 1615665) [1..1000]) > 50