module Entity.Player where

import qualified Level as L
import qualified Entity.Mob as M

-- | A Name, with surname.
data Name = Name String String
          deriving (Eq, Show)

-- | Gender of a player.
data Gender = Masculine | Feminine
            deriving (Eq, Show)

-- | What a Player can be:
data Player = Human      -- ^ Normal Human.
            | Mob M.Mob  -- ^ Some mob.
            deriving (Eq, Show)

-- | All the specs about the player.
data PlayerData = PlayerData
    { pName         :: Name        -- ^ initial Name, with surname.
    , pAge          :: Int         -- ^ Initial Age, in years
    , pGender       :: Gender      -- ^ Initial Gender
    , pHp           :: Int         -- ^ Initial Health Points.
    , pIntelligence :: L.Experience  -- ^ Initial Intelligence.
    , pStrengh      :: L.Experience  -- ^ Initial Strengh.
    } deriving (Eq, Show)