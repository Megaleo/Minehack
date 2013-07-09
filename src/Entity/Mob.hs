module Entity.Mob where

import qualified Level as L

-- | All the Mobs:
data Mob = Horse   -- ^ A Horse
         | Olimpio -- ^ "O Destruidor"
         deriving (Eq, Show)

-- | Initial Health Points.
initHp :: Mob -> Int
initHp Horse   = 50
initHp Olimpio = 999999999999999999

-- | Initial Intelligence.
initIntelligence :: Mob -> L.Experience
initIntelligence Horse   = fst . L.toExpBound $ L.Level 3
initIntelligence Olimpio = L.Experience 999999999999999999

-- | Initial Strengh.
initStrengh :: Mob -> L.Experience
initStrengh Horse   = fst . L.toExpBound $ L.Level 10
initStrengh Olimpio = L.Experience 999999999999999999