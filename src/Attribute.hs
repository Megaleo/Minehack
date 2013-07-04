module Attribute where
-- All the attributes given to a Tile (TileType) by something or someone in-game
--
-- #@ Examples @#
--
-- -# Fire Resistance #-
-- @ Gives you resistance to fire
-- @ May be given by a potion
--
-- -# Illness #-
-- @ Slowly removes your HP
-- @ May be given randomly in some circumstances
--
-- -# Fatique #-
-- @ Slows down your moviments and attacks
-- @ May be given by a potion or running without resting/eating

data Attribute = NoAttributes
               | Burning
               deriving (Eq, Enum)  

-- | Attribute's unique name.
name              :: Attribute -> String
name NoAttributes = "NoAttributes"
name Burning      = "Burning"

instance Show Attribute where
  showsPrec _ = showString . name

-- -- | Attribute's effects.
--effects :: [Effect]
