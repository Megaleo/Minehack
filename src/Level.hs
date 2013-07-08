module Level where

-- | Experience points.
newtype Experience = Experience Int
                   deriving (Eq, Show)

-- | Levels are calculated from experience points
-- using the function toLevel.
newtype Level = Level Int
              deriving (Eq, Show)

-- | The calculation for the Level.
-- Basically, is log base 2.
toLevel :: Experience -> Level
toLevel (Experience 0)  = error "Zero Experience"
toLevel (Experience xp) = Level $ floor $ logBase 2 (toEnum xp :: Double)

-- | Returns the range of experience a level cover.
toExpBound :: Level -> (Experience, Experience)
toExpBound (Level lvl) = (Experience (2 ^ lvl), Experience (2 ^ (lvl + 1) - 1 ))