module Block where
-- "Block.RawMaterial" for block's raw materials (There are blocks!)
--
-- Here will be all the definitions and functions around the concept of the Block.

import qualified Random as R

data Block = Air 
           | Wood
           deriving Eq

-- | Block's visibility.
seeThrough       :: Block -> Bool --seeThrough :: Block -> ColorSpectrum -> Bool
seeThrough Air   = True
seeThrough Wood  = False

-- | Block's "walk and stand over" caracteristic (with damage value).
-- | The Maybe is for specification if you can not walk through (Nothing)
-- | or if you can actually walk over the block but it (the block) takes some damage.
walkThrough      :: Block -> Maybe R.Roll
walkThrough Air  = Just $ R.Fix 0
walkThrough Wood = Just $ R.Roll 1 2

-- | Block's amount of hits to break it (initial damage value).
-- | If its "Nothing", then the block is indestructible or it can't
-- | be broken, like Air
hitstoBreak      :: Block -> Maybe R.Roll
hitstoBreak Air  = Nothing
hitstoBreak Wood = Just $ R.Roll 15 20  

-- | Block's weight in grams per m^3
-- | Will be specified the amount of grams per m^3 the player (or anything) can 
-- | support. In normal conditions, is 1225 g/m^3, the "weight" of Air
weight           :: Block -> Double -- weight :: Block -> Block -> Weight
weight Air       = 1225.0 -- Source: http://en.wikipedia.org/wiki/Density_of_air
weight Wood      = 700    -- Source: http://en.wikipedia.org/wiki/Density

-- | Block's drop when broken.
drops           :: Block -> Maybe [Block]
drops Air       = Nothing
drops Wood      = Nothing

-- -- | Block's physical material.
-- material    :: Block -> [Block.RawMaterial]



{-
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

-}