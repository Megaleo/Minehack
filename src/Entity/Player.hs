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

-- | All the data about the player.
-- This will go on its attributes.
data PlayerData = PlayerData
    { pName         :: Name          -- ^ Name, with surname.
    , pAge          :: Int           -- ^ Age, in years
    , pGender       :: Gender        -- ^ Gender
    , pHp           :: Int           -- ^ Health Points.
    , pMaxHp        :: Int           -- ^ Maximum HP
    , pIntelligence :: L.Experience  -- ^ Intelligence.
    , pStrengh      :: L.Experience  -- ^ Strengh.
    } deriving (Eq, Show)
