module Attribute where
-- All the attributes given to a Tile (TileType) by something or someone in-game.
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

import qualified Tile.TileType as TT
import qualified Entity.Mob as EM
import qualified Entity.Player as EP

-- All the Attributes:
data Attribute = NoAttributes
               | Burning
               | Broke
               | PData EP.PlayerData
               | MData EM.MobData
               deriving (Eq, Show)

-- | Returns the ID of an Attribute, if it has an
-- an argument, then it is returned in the meta-string.
attrID :: Attribute -> TT.ID
attrID NoAttributes  = TT.ID 0 ""
attrID Burning       = TT.ID 1 ""
attrID Broke         = TT.ID 2 ""
attrID (PData pData) = TT.ID 3 $ show pData
attrID (MData mData) = TT.ID 4 $ show mData

-- | Returns the attribute related to
-- some specific id. If it doesn't exist
-- then 'Nothing' is returned.
fromAttrID :: TT.ID -> Maybe Attribute
fromAttrID (TT.ID 0 _) = Just NoAttributes
fromAttrID (TT.ID 1 _) = Just Burning
fromAttrID (TT.ID 2 _) = Just Broke
fromAttrID (TT.ID 3 p) = Just $ PData (read p :: EP.PlayerData)
fromAttrID (TT.ID 4 m) = Just $ MData (read m :: EM.MobData)
fromAttrID _           = Nothing

-- | Verify if an attribute is a Data,
-- a 'PlayerData' or a 'MobData'.
isSomeData :: Attribute -> Bool
isSomeData (PData _) = True
isSomeData (MData _) = True
isSomeData _         = False

-- | Find the Data in a list of attributes,
-- if it don't find, then 'NoAttributes' is
-- returned.
findSomeData :: [Attribute] -> Attribute
findSomeData ((PData p) : _) = PData p
findSomeData ((MData m) : _) = MData m
findSomeData (_ : ps)        = findSomeData ps
findSomeData []              = NoAttributes

-- | Adds an amount of HP to a Data attribute.
addHp :: Attribute -> Int -> Attribute
addHp (PData p) hp = PData $ EP.addPlayerHp p hp
addHp (MData m) hp = MData $ EM.addMobHp m hp
addHp attr _       = attr

-- | Adds an attribute to a list of it, but some
-- attributes will not be repeated if they already
-- exists in the list.
addAttribute :: Attribute -> [Attribute] -> [Attribute]
addAttribute Broke attrs   = if any (== Broke) attrs then attrs else Broke : attrs
addAttribute Burning attrs = if any (== Burning) attrs then attrs else Burning : attrs
addAttribute a attrs       = a : attrs
