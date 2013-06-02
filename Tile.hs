module Tile where
-- "TileType.Flammable"   for flammable tiles
-- "TileType.Blessedness" for (bless/curse)able tiles
-- "TileType.Worth"       for valuable tiles

import System.IO
import Control.Monad
import Data.Array

import qualified Attribute as A

-- Basically, what can be Tile (e.g. Block, Item, Moster, Player, etc.)
class TileType a where

--Tile's unique ID
  id     :: a -> Int

--Tile's unique symbol
  symbol :: a -> Char

--Tile's unique name
  name   :: a -> String

-- Tile is the representation of each piece in a world, it has a type (Block, Item or Player) and
-- their own Attributes (Attribute) that specify the aspects of a tile in the world, like the block
-- damage or the intelligence level of a Player.
data Tile a b where
  Tile  :: (TileType a, A.Attribute b) => a -> [b] -> Tile a [b]
  Above :: (TileType a, A.Attribute b) => Tile a [b] -> Tile a [b] -- One tile above the other
