-- An Entity is anything that can provoke and generate actions by itself.
-- Entities are players, NPC's, monsters, animals, self-playing pianos, etc.

module Entity where

import qualified Entity.Player as P
import qualified Entity.Mob as M

data Entity = EPlayer P.Player  -- ^ A player (User).
            | EMob M.Mob        -- ^ A Mob, peaceful or not.
            deriving (Eq, Show)