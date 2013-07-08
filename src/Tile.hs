module Tile where

import qualified Tile.TileType as TT
import qualified Attribute as A

-- | Tile is the representation of each piece in a world, it has
-- a type (Block, Item or Player) and their own Attributes that
-- specify the aspects of a tile in the world, like the intelligence
-- level of a Player or Monster.
data Tile =
  Tile TT.TileType [A.Attribute]
  -- Above Tile Tile
  deriving (Show, Eq)

-- | Symbol of a Tile, based on its TileType
tileSymbol :: Tile -> Char
tileSymbol (Tile tileType _) = TT.symbol tileType