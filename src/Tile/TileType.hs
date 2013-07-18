-- "Tile.TileType.Flammable"   for flammable tiles.
-- "Tile.TileType.Blessedness" for (bless/curse)able tiles.
-- "Tile.TileType.Worth"       for valuable tiles.

module Tile.TileType where

import qualified Block as B
import qualified Item as I
import qualified Entity as E
import qualified Entity.Player as EP
import qualified Entity.Mob as EM

data TileType = TBlock B.Block
              | TItem  I.Item
              | TEntity E.Entity
              deriving (Eq, Show, Read)

-- A ID is a Number a meta-string
data ID = ID Int String
        deriving (Eq, Show, Read)

-- | TileType's unique ID.
tileID :: TileType -> ID
tileID (TBlock B.Air)                     = ID 0 ""
tileID (TBlock B.Wood)                    = ID 1 ""
tileID (TItem I.Wood)                     = ID 2 ""
tileID (TEntity (E.EPlayer EP.Human))     = ID 3 ""
tileID (TEntity (E.EPlayer (EP.Mob mob))) = ID 3 $ show $ tileID $ TEntity $ E.EMob mob
tileID (TEntity (E.EMob EM.Horse))        = ID 4 ""
tileID (TEntity (E.EMob EM.Olimpio))      = ID 5 ""
tileID (TBlock B.Water)                   = ID 6 ""
tileID (TBlock B.DeepWater)               = ID 7 ""
tileID (TBlock B.Sand)                    = ID 8 ""

-- | Converts from an ID, it can allow less
-- data storage when saving chunks.
fromTileID :: ID -> Maybe TileType
fromTileID (ID 0 _)       = Just $ TBlock B.Air
fromTileID (ID 1 _)       = Just $ TBlock B.Wood
fromTileID (ID 2 _)       = Just $ TItem I.Wood
fromTileID (ID 3 "")      = Just $ TEntity $ E.EPlayer EP.Human
fromTileID (ID 3 mobId)
    | fromTileID (read mobId :: ID) == Nothing = Nothing
    | otherwise = Just $ TEntity $ E.EPlayer $ EP.Mob $ returnMob $ fromTileID (read mobId :: ID)
        where
            -- | Returns the mob of a player from a TileType Entity.
            returnMob (Just (TEntity (E.EMob mob))) = mob
            returnMob _                             = error "It is impossible to get here"
fromTileID (ID 4 _)       = Just $ TEntity $ E.EMob EM.Horse
fromTileID (ID 5 _)       = Just $ TEntity $ E.EMob EM.Olimpio
fromTileID _ = Nothing
fromTileID (ID 6 _)       = Just $ TBlock B.Water
fromTileID (ID 7 _)       = Just $ TBlock B.DeepWater
fromTileID (ID 8 _)       = Just $ TBlock B.Sand


-- | TileType's unique symbol.
symbol :: TileType -> Char
symbol (TBlock B.Air)                     = '.'
symbol (TBlock B.Wood)                    = 'W'
symbol (TItem I.Wood)                     = 'w'
symbol (TEntity (E.EPlayer EP.Human))     = '@'
symbol (TEntity (E.EPlayer (EP.Mob mob))) = symbol $ TEntity $ E.EMob mob
symbol (TEntity (E.EMob EM.Horse))        = 'u'
symbol (TEntity (E.EMob EM.Olimpio))      = 'O'
symbol (TBlock B.Water)                   = '='
symbol (TBlock B.DeepWater)               = '~'
symbol (TBlock B.Sand)                    = '_'

-- | TileType's unique name.
name :: TileType -> String
name (TBlock B.Air)                     = "Air"
name (TBlock B.Wood)                    = "Block of Wood"
name (TItem I.Wood)                     = "Item of Wood"
name (TEntity (E.EPlayer EP.Human))     = "Human"
name (TEntity (E.EPlayer (EP.Mob mob))) = name $ TEntity $ E.EMob mob
name (TEntity (E.EMob EM.Horse))        = "Horse"
name (TEntity (E.EMob EM.Olimpio))      = "O Destruidor"
name (TBlock B.Water)                   = "Water"
name (TBlock B.DeepWater)               = "Deep Water"
name (TBlock B.Sand)                    = "Sand"

-- | (Initial) Weight of a TileType, in grams:
-- In case of a Block, is measured in g/m^3.
-- In case of a Player, the initial weight is measured.
-- In case of an Item, is just the individual weight.
weight :: TileType -> Double
weight (TBlock B.Air)                     = 1225.0   -- Source: http://en.wikipedia.org/wiki/Density_of_air
weight (TBlock B.Wood)                    = 700000.0 -- Source: http://en.wikipedia.org/wiki/Density
weight (TItem I.Wood)                     = 1000.0
weight (TEntity (E.EPlayer EP.Human))     = 70000.0
weight (TEntity (E.EPlayer (EP.Mob mob))) = weight $ TEntity $ E.EMob mob
weight (TEntity (E.EMob EM.Horse))        = 400.0 -- Source: http://en.wikipedia.org/wiki/Horse
weight (TEntity (E.EMob EM.Olimpio))      = 1 / 0
weight (TBlock B.Water)                   = 1000000.0 -- Source: http://en.wikipedia.org/wiki/Water
weight (TBlock B.DeepWater)               = B.blockWeight B.Water
weight (TBlock B.Sand)                    = 2330.0    -- Source: http://en.wikipedia.org/wiki/Density

-- | Returns the damage point a TileType
-- will cause if it is used as a weapon.
damageAsWeapon :: TileType -> Int
damageAsWeapon (TBlock B.Wood) = -100 -- ^ A Block of Wood is heavy!
damageAsWeapon (TItem I.Wood)  = -10
damageAsWeapon _               = 0

-- | Returns if a TileType is an Entity
isOnlyEntity :: TileType -> Bool
isOnlyEntity (TEntity _) = True
isOnlyEntity _           = False

-- | Returns if a TileType is a Mob
isOnlyMob :: TileType -> Bool
isOnlyMob (TEntity (E.EMob _)) = True
isOnlyMob _                    = False

-- | Returns if a TileType is a Player
isOnlyPlayer :: TileType -> Bool
isOnlyPlayer (TEntity (E.EPlayer _)) = True
isOnlyPlayer _                       = False

-- | Returns if a TileType is a Block
isOnlyBlock :: TileType -> Bool
isOnlyBlock (TBlock _) = True
isOnlyBlock _          = False

-- | Returns if a TileType is an Item
isOnlyItem :: TileType -> Bool
isOnlyItem (TItem _) = True
isOnlyItem _         = False
