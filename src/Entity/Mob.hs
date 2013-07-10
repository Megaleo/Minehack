module Entity.Mob where

import qualified Level as L

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
    { mName         :: Maybe String     -- ^ Maybe a name without surname.
    , mAge          :: Maybe Int        -- ^ Maybe an age in years
    , mGender       :: Maybe MobGender  -- ^ Maybe a MobGender
    , mHp           :: Int              -- ^ Health Points.
    , mMaxHp        :: Int              -- ^ Maximum HP
    , mIntelligence :: L.Experience     -- ^ Intelligence.
    , mStrengh      :: L.Experience     -- ^ Strengh.
    } deriving (Eq, Show, Read)

changeMobName :: MobData -> Maybe String -> MobData
changeMobName (MobData _ a g h mh i s) name = MobData name a g h mh i s

changeMobAge :: MobData -> Maybe Int -> MobData
changeMobAge (MobData n _ g h mh i s) age = MobData n age g h mh i s

addMobAge :: MobData -> Int -> MobData
addMobAge (MobData n a g h mh i s) age = MobData n (fmap (+age) a) g h mh i s

changeMobGender :: MobData -> Maybe MobGender -> MobData
changeMobGender (MobData n a _ h mh i s) gender = MobData n a gender h mh i s

changeMobHp :: MobData -> Int -> MobData
changeMobHp (MobData n a g _ mh i s) hp = MobData n a g hp mh i s

addMobHp :: MobData -> Int -> MobData
addMobHp (MobData n a g h mh i s) hp = MobData n a g (h + hp) mh i s

changeMobMaxHP :: MobData -> Int -> MobData
changeMobMaxHP (MobData n a g h _ i s) maxHp = MobData n a g h maxHp i s

addMobMaxHP :: MobData -> Int -> MobData
addMobMaxHP (MobData n a g h mh i s) maxHp = MobData n a g h (mh + maxHp) i s

changeMobIntelligence :: MobData -> L.Experience -> MobData
changeMobIntelligence (MobData n a g h mh _ s) intelligence = MobData n a g h mh intelligence s

changeMobStrengh :: MobData -> L.Experience -> MobData
changeMobStrengh (MobData n a g h mh i _) strengh = MobData n a g h mh i strengh

initMobData :: Mob -> MobData
initMobData Horse = MobData { mName         = Nothing
                            , mAge          = Just 25
                            , mGender       = Nothing
                            , mHp           = 300
                            , mMaxHp        = 300
                            , mIntelligence = fst . L.toExpBound $ L.Level 3
                            , mStrengh      = fst . L.toExpBound $ L.Level 10
                            }

initMobData Olimpio = MobData { mName         = Just "Olimpio"
                              , mAge          = Just 999999999999999999
                              , mGender       = Just Intersex
                              , mHp           = 999999999999999999
                              , mMaxHp        = 999999999999999999
                              , mIntelligence = L.Experience 999999999999999999
                              , mStrengh      = L.Experience 999999999999999999
                              }
