module Block where
-- "Block.RawMaterial" for block's raw materials (There are blocks!)
--
-- Here will be all the definitions and functions around the concept of the Block.

import qualified Random as R

-- All the Blocks:
data Block = Air
           | Wood
           deriving Eq

-- | All these functions below have to handle every
-- single blockin the Block declaration.

-- | Block's visibility.
seeThrough :: Block -> Bool --seeThrough :: Block -> ColorSpectrum -> Bool
seeThrough Air   = True
seeThrough Wood  = False

-- | Block's "walk and stand over" caracteristic (with damage value).
-- The Maybe is for specification if you can not walk through (Nothing)
-- or if you can actually walk over the block but it (the block) takes some damage.
walkThrough :: Block -> Maybe R.Roll
walkThrough Air  = Just $ R.Fix 0
walkThrough Wood = Just $ R.Roll 1 2

-- | Block's amount of hits to break it (initial damage value).
-- If its "Nothing", then the block is indestructible or it can't
-- be broken, like Air.
hitstoBreak :: Block -> Maybe R.Roll
hitstoBreak Air  = Nothing
hitstoBreak Wood = Just $ R.Roll 15 20

-- | Block's weight in grams per m^3
-- Will be specified the amount of grams per m^3 the player (or anything) can
-- support. In normal conditions, is 1225 g/m^3, the "weight" of Air.
weight :: Block -> Double
weight Air       = 1225.0 -- Source: http://en.wikipedia.org/wiki/Density_of_air
weight Wood      = 700    -- Source: http://en.wikipedia.org/wiki/Density

-- | Block's drops when broken.
drops :: Block -> Maybe [Block]
drops Air       = Nothing
drops Wood      = Nothing

-- -- | Block's physical material.
-- material    :: Block -> [Block.RawMaterial]