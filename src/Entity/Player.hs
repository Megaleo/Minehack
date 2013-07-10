module Entity.Player where

import qualified Level as L
import qualified Entity.Mob as M

-- | A Name, with surname.
data Name = Name String String
          deriving (Eq, Show, Read)

-- | Gender of a player.
data Gender = Masculine | Feminine | Intersex
            deriving (Eq, Show, Read)

-- | What a Player can be:
data Player = Human      -- ^ Normal Human.
            | Mob M.Mob  -- ^ Some mob.
            deriving (Eq, Show)

initPlayerData :: Player -> PlayerData
initPlayerData Human = PlayerData { pName         = Name "Wallace" "Ferner"
                                  , pAge          = 25
                                  , pGender       = Masculine
                                  , pHp           = 100
                                  , pMaxHp        = 100
                                  , pIntelligence = fst . L.toExpBound $ L.Level 1
                                  , pStrengh      = fst . L.toExpBound $ L.Level 1
                                  }
initPlayerData (Mob mob) = PlayerData { pName         = Name (show mob) "McMonster"
                                      , pAge          = 0
                                      , pGender       = Intersex
                                      , pHp           = 100
                                      , pMaxHp        = 100
                                      , pIntelligence = fst . L.toExpBound $ L.Level 1
                                      , pStrengh      = fst . L.toExpBound $ L.Level 1
                                      }

-- | All the specs about the player.
data PlayerData = PlayerData
    { pName         :: Name          -- ^ Name, with surname.
    , pAge          :: Int           -- ^ Age, in years
    , pGender       :: Gender        -- ^ Gender
    , pHp           :: Int           -- ^ Health Points.
    , pMaxHp        :: Int           -- ^ Maximum HP
    , pIntelligence :: L.Experience  -- ^ Intelligence.
    , pStrengh      :: L.Experience  -- ^ Strengh.
    } deriving (Eq, Show, Read)

-- | Changes the player's name.
changePlayerName :: PlayerData -> Name -> PlayerData
changePlayerName (PlayerData _ a g h mh i s) name = PlayerData name a g h mh i s

-- | Changes the player's age.
changePlayerAge :: PlayerData -> Int -> PlayerData
changePlayerAge (PlayerData n _ g h mh i s) age = PlayerData n age g h mh i s

-- | Adds to the player's age.
addPlayerAge :: PlayerData -> Int -> PlayerData
addPlayerAge (PlayerData n a g h mh i s) age = PlayerData n (a + age) g h mh i s

-- | Changes the player's gender.
changePlayerGender :: PlayerData -> Gender -> PlayerData
changePlayerGender (PlayerData n a _ h mh i s) gender = PlayerData n a gender h mh i s

-- | Changes the player's HP.
changePlayerHp :: PlayerData -> Int -> PlayerData
changePlayerHp (PlayerData n a g _ mh i s) hp = PlayerData n a g hp mh i s

-- | Adds to the player's HP.
addPlayerHp :: PlayerData -> Int -> PlayerData
addPlayerHp (PlayerData n a g h mh i s) hp = PlayerData n a g (h + hp) mh i s

-- | Changes the player's Maximum HP.
changePlayerMaxHP :: PlayerData -> Int -> PlayerData
changePlayerMaxHP (PlayerData n a g h _ i s) maxHp = PlayerData n a g h maxHp i s

-- | Adds to the player's Maximum HP.
addPlayerMaxHP :: PlayerData -> Int -> PlayerData
addPlayerMaxHP (PlayerData n a g h mh i s) maxHp = PlayerData n a g h (mh + maxHp) i s

-- | Changes the player's intelligence.
changePlayerIntelligence :: PlayerData -> L.Experience -> PlayerData
changePlayerIntelligence (PlayerData n a g h mh _ s) intelligence = PlayerData n a g h mh intelligence s

-- | Changes the player's strengh.
changePlayerStrengh :: PlayerData -> L.Experience -> PlayerData
changePlayerStrengh (PlayerData n a g h mh i _) strengh = PlayerData n a g h mh i strengh