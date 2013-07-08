-- "TileType.Flammable"   for flammable tiles.
-- "TileType.Blessedness" for (bless/curse)able tiles.
-- "TileType.Worth"       for valuable tiles.

module Tile where

import qualified Attribute as A
import qualified Block as B
import qualified Item as Item
import qualified Entity as E
import qualified Entity.Player as EP
import qualified Entity.Mob as EM

data TileType = TBlock B.Block
              | TItem  I.Item
              | TEntity E.Entity
              deriving Eq

-- A ID is a Number a meta-string
data ID = ID Int (Maybe String)
        deriving (Eq, Show, Read)

-- | TileType's unique ID.
tileId :: TileType -> ID
tileId (TBlock B.Air)                     = ID 0 Nothing
tileId (TBlock B.Wood)                    = ID 1 Nothing
tileId (TItem I.Wood)                     = ID 1 $ Just "Item"
tileId (TEntity (E.EPlayer EP.Human))     = ID 2 Nothing
tileId (TEntity (E.EPlayer (EP.Mob mob))) = ID 2 $ Just $ show $ tileId $ TEntity $ E.EMob mob
tileId (TEntity (E.EMob EM.Horse))        = ID 3 Nothing
tileId (TEntity (E.EMob EM.Olimpio))      = ID 4 Nothing

-- | Converts from an ID, it can allow less
-- data storage when saving chunks.
fromTileId :: ID -> Maybe TileType
fromTileId (ID 0 Nothing)       = Just $ TBlock B.Air
fromTileId (ID 1 Nothing)       = Just $ TBlock B.Wood
fromTileId (ID 1 (Just "Item")) = Just $ TItem I.Wood
fromTileId (ID 2 Nothing)       = Just $ TEntity $ E.EPlayer EP.Human
fromTileId (ID 2 (Just mobId))
    | fromTileId (read mobId :: ID) == Nothing = Nothing
    | otherwise = Just $ TEntity $ E.EPlayer $ EP.Mob $ returnMob $ fromTileId (read mobId :: ID)
        where
            returnMob (Just (TEntity (E.EMob mob))) = mob
            returnMob _                             = error "It is impossible to get here"
fromTileId (ID 3 Nothing)       = Just $ TEntity $ E.EMob EM.Horse
fromTileId (ID 4 Nothing)       = Just $ TEntity $ E.EMob EM.Olimpio
fromTileId _ = Nothing


-- | TileType's unique symbol.
symbol :: TileType -> Char
symbol (TBlock B.Air)                     = '.'
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

-- | Show instance for TileType
instance Show TileType where
  showsPrec _ = showString . name

-- | Tile is the representation of each piece in a world, it has
-- a type (Block, Item or Player) and their own Attributes that
-- specify the aspects of a tile in the world, like the intelligence
-- level of a Player or Monster.
data Tile =
  Tile TileType [A.Attribute]
  -- Above Tile Tile
  deriving (Show, Eq)

tileSymbol :: Tile -> Char
tileSymbol (Tile tileType _) = symbol tileType