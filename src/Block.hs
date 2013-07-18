module Block where
-- "Block.RawMaterial" for block's raw materials (There are blocks!)
--
-- Here will be all the definitions and functions around the concept of the Block.

import qualified Random as R

-- | All the Blocks:
data Block = Air   -- ^ Air, just air.
           | Wood  -- ^ Solid block of wood.
           | Water
           | DeepWater
           | Sand
           deriving (Eq, Show, Read)

-- | All these functions below have to handle every
-- single blockin the Block declaration.

-- | Block's visibility.
seeThrough :: Block -> Bool --seeThrough :: Block -> ColorSpectrum -> Bool
seeThrough Air       = True
seeThrough Wood      = False
seeThrough Water     = False
seeThrough DeepWater = False
seeThrough Sand      = False

-- | Block's "walk and stand over" caracteristic (with damage value).
-- The Maybe is for specification if you can not walk through (Nothing)
-- or if you can actually walk over the block but it (the block) takes some damage.
walkThrough :: Block -> Maybe R.Roll
walkThrough Air       = Just $ R.Fix 0
walkThrough Wood      = Just $ R.Roll 1 2
walkThrough Water     = Just $ R.Fix 0
walkThrough DeepWater = Nothing
walkThrough Sand      = Just $ R.Fix 0

-- | If something can stay inside of a block.
stayInside :: Block -> Bool
stayInside Air       = True
stayInside Wood      = False
stayInside Water     = True
stayInside DeepWater = False
stayInside Sand      = False

-- | Block's amount of hits to break it (initial damage value).
-- If its "Nothing", then the block is indestructible or it can't
-- be broken, like Air.
hitstoBreak :: Block -> Maybe R.Roll
hitstoBreak Air       = Nothing
hitstoBreak Wood      = Just $ R.Roll 15 20
hitstoBreak Water     = Nothing
hitstoBreak DeepWater = Nothing
hitstoBreak Sand      = Nothing

-- | Block's weight in grams per m^3
-- Will be specified the amount of grams per m^3 the player (or anything) can
-- support. In normal conditions, is 1225 g/m^3, the "weight" of Air.
blockWeight :: Block -> Double
blockWeight Air       = 1225.0    -- Source: http://en.wikipedia.org/wiki/Density_of_air
blockWeight Wood      = 700       -- Source: http://en.wikipedia.org/wiki/Density
blockWeight Water     = 1000000.0 -- Source: http://en.wikipedia.org/wiki/Water
blockWeight DeepWater = blockWeight Water
blockWeight Sand      = 2330.0    -- Source: http://en.wikipedia.org/wiki/Density

-- | Block's drops when broken.
drops :: Block -> Maybe [Block]
drops Air       = Nothing
drops Wood      = Nothing
drops Water     = Nothing
drops DeepWater = Nothing
drops Sand      = Nothing

-- -- | Block's physical material.
-- material    :: Block -> [Block.RawMaterial]