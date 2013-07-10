module Entity.Mob where

import qualified Level as L
import qualified Random as R

-- | All the Mobs:
data Mob = Horse   -- ^ A Horse
         | Olimpio -- ^ "O Destruidor"
         deriving (Eq, Show)

-- | Gender of a mob.
data MobGender = Masculine | Feminine | Intersex
               deriving (Eq, Show, Read)

-- | All the data about the mob.
-- This will go on its attributes.
data MobData = MobData
    { mNameGender   :: R.OneOf (String, MobGender)  -- ^ Maybe a name without surname, and a gender to the name.
    , mAge          :: Maybe Int                    -- ^ Maybe an age in years
    , mHp           :: Int                          -- ^ Health Points.
    , mMaxHp        :: Int                          -- ^ Maximum HP
    , mIntelligence :: L.Experience                 -- ^ Intelligence.
    , mStrengh      :: L.Experience                 -- ^ Strengh.
    } deriving (Eq, Show, Read)

-- | Changes the mob's name and gender.
changeMobNameGender :: MobData -> R.OneOf (String, MobGender) -> MobData
changeMobNameGender (MobData _ a h mh i s) name = MobData name a h mh i s

-- | Changes the mob's age.
changeMobAge :: MobData -> Maybe Int -> MobData
changeMobAge (MobData n _ h mh i s) age = MobData n age h mh i s

-- | Adds to the mob's age.
addMobAge :: MobData -> Int -> MobData
addMobAge (MobData n a h mh i s) age = MobData n (fmap (+age) a) h mh i s

-- | Changes the mob's HP.
changeMobHp :: MobData -> Int -> MobData
changeMobHp (MobData n a _ mh i s) hp = MobData n a hp mh i s

-- | Adds to the mob's HP.
addMobHp :: MobData -> Int -> MobData
addMobHp (MobData n a h mh i s) hp = MobData n a (h + hp) mh i s

-- | Changes the mob's Maximum HP.
changeMobMaxHP :: MobData -> Int -> MobData
changeMobMaxHP (MobData n a h _ i s) maxHp = MobData n a h maxHp i s

-- | Adds to the mob's Maximum HP.
addMobMaxHP :: MobData -> Int -> MobData
addMobMaxHP (MobData n a h mh i s) maxHp = MobData n a h (mh + maxHp) i s

-- | Changes the mob's intelligence.
changeMobIntelligence :: MobData -> L.Experience -> MobData
changeMobIntelligence (MobData n a h mh _ s) intelligence = MobData n a h mh intelligence s

-- | Changes the mob's strengh.
changeMobStrengh :: MobData -> L.Experience -> MobData
changeMobStrengh (MobData n a h mh i _) strengh = MobData n a h mh i strengh

spawnMob :: Int -> MobData -> MobData
spawnMob seed mob = changeMobNameGender mob $ R.Elem $ R.randomOneOf seed $ mNameGender mob

initMobData :: Mob -> MobData
initMobData Horse = MobData { mNameGender   = R.OneOf [ ("Rosie", Feminine)
                                                      , ("Jack", Masculine)
                                                      , ("George", Masculine)
                                                      , ("Millie", Feminine)
                                                      , ("Spirit", Masculine)
                                                      , ("Star", Feminine)
                                                      ] -- ^ Source: http://www.horsemart.co.uk/horse_advice/your-top-500-most-popular-horses-names/1697
                            , mAge          = Just 25
                            , mHp           = 300
                            , mMaxHp        = 300
                            , mIntelligence = fst . L.toExpBound $ L.Level 3
                            , mStrengh      = fst . L.toExpBound $ L.Level 10
                            }

initMobData Olimpio = MobData { mNameGender   = R.Elem ("Olimpio", Masculine)
                              , mAge          = Just 999999999999999999
                              , mHp           = 999999999999999999
                              , mMaxHp        = 999999999999999999
                              , mIntelligence = L.Experience 999999999999999999
                              , mStrengh      = L.Experience 999999999999999999
                              }
