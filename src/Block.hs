module Block where
-- "Block.RawMaterial" for block's raw materials (There are blocks!)
--
-- Here will be all the definitions and functions around the concept of the Block.

import System.IO
import Control.Monad
import Data.Array

import qualified Tile as T
import qualified Random as R

-- | Characteristics to be an Block (class or data?).
class T.TileType a => Block a where
  -- | Block's visibility.
  seeThrough  :: a -> Bool --seeThrough :: a -> ColorSpectrum -> Bool
  -- | Block's "walk and stand over" caracteristic (with damage value).
  walkThrough :: a -> Maybe R.Roll
  -- | Block's fixed amount of hits to break (initial damage value).
  hitstoBreak :: a -> R.Roll
  -- | Block's weight.
  weight      :: a -> Int -- weight :: a -> a -> Weight
  -- | Block's drop when broken.
  drops       :: a -> Maybe [a]
  -- -- | Block's physical material.
  -- material    :: a -> [Block.RawMaterial]

-- Blocks for Testing Purposes

data Air = Air deriving (Show, Eq)
instance T.TileType Air where
  id _     = 0
  name _   = "Air"
  symbol _ = ' '
  spawnCond _ _ _ = True
instance Block Air where
  seeThrough _  = True
  walkThrough _ = Just (R.Fix 0)
  hitstoBreak _ = R.Fix 0
  weight _      = 0
  drops _       = Just [Air]

