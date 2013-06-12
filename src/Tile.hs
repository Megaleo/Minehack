module Tile where
-- "TileType.Flammable"   for flammable tiles
-- "TileType.Blessedness" for (bless/curse)able tiles
-- "TileType.Worth"       for valuable tiles

import System.IO
import Control.Monad
import Data.Array

import qualified Attribute as A

type Coord = (Int, Int)

-- Basically, what can be Tile (e.g. Block, Item, Moster, Player, etc.)
class (Show a, Eq a) => TileType a where
--Tile's unique ID
  id        :: a -> Int
--Tile's unique symbol
  symbol    :: a -> Char
--Tile's unique name
  name      :: a -> String
-- Condition to Spawn in the world
  spawnCond :: a -> Int -> Coord -> Bool

-- Horrible intances for "spawnCond" function
--
-- meteficha: *horrible* indeed!
instance Show (a -> b) where
  showsPrec _ _ = showString "Função"

instance Eq (a -> b) where
  _ == _ = True

-- Datatype for TileType class
data DataTileType = TileType { tId        :: Int
                             , tSymbol    :: Char
                             , tName      :: String
                             , tSpawnCond :: Int -> Coord -> Bool} deriving (Show, Eq)

-- Coverts a TileType to a DataTileType
tileTypeData :: TileType a => a -> DataTileType
tileTypeData tile = TileType { tId        = Tile.id tile
                             , tSymbol    = symbol tile
                             , tName      = name tile
                             , tSpawnCond = spawnCond tile}

-- "Converts" DataTileType to a TileType
instance TileType DataTileType where
  id        (TileType tId _ _ _)        = tId
  symbol    (TileType _ tSymbol _ _)    = tSymbol
  name      (TileType _ _ tName _)      = tName
  spawnCond (TileType _ _ _ tSpawnCond) = tSpawnCond

-- Tile is the representation of each piece in a world, it has a type (Block, Item or Player) and
-- their own Attributes that specify the aspects of a tile in the world, like the block
-- damage or the intelligence level of a Player.
data Tile where
  Tile  :: DataTileType -> [A.DataAttribute] -> Tile
--Above :: Tile -> Tile -> Tile
  deriving (Show, Eq)

