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
              deriving Eq

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

-- | Converts from an ID, it can allow less
-- data storage when saving chunks.
fromTileID :: ID -> Maybe TileType
fromTileID (ID 0 _)       = Just $ TBlock B.Air
fromTileID (ID 1 _)       = Just $ TBlock B.Wood
fromTileID (ID 2 _)       = Just $ TItem I.Wood
fromTileID (ID 3 "")       = Just $ TEntity $ E.EPlayer EP.Human
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


-- | TileType's unique symbol.
symbol :: TileType -> Char
symbol (TBlock B.Air)                     = ' '
symbol (TBlock B.Wood)                    = 'W'
symbol (TItem I.Wood)                     = 'w'
symbol (TEntity (E.EPlayer EP.Human))     = '@'
symbol (TEntity (E.EPlayer (EP.Mob mob))) = symbol $ TEntity $ E.EMob mob
symbol (TEntity (E.EMob EM.Horse))        = 'u'
symbol (TEntity (E.EMob EM.Olimpio))      = 'O'

-- | TileType's unique name.
name :: TileType -> String
name (TBlock B.Air)                     = "Air"
name (TBlock B.Wood)                    = "Block of Wood"
name (TItem I.Wood)                     = "Item of Wood"
name (TEntity (E.EPlayer EP.Human))     = "Human"
name (TEntity (E.EPlayer (EP.Mob mob))) = name $ TEntity $ E.EMob mob
name (TEntity (E.EMob EM.Horse))        = "Horse"
name (TEntity (E.EMob EM.Olimpio))      = "O Destruidor"

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

damageAsWeapon :: TileType -> Int
damageAsWeapon (TBlock B.Wood) = -100 -- ^ A Block of Wood is heavy!
damageAsWeapon (TItem I.Wood)  = -10
damageAsWeapon _               = 0

-- | Show instance for TileType
instance Show TileType where
  showsPrec _ = showString . name
