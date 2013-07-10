module Tile where

import qualified Tile.TileType as TT
import qualified Attribute as A

-- | Tile is the representation of each piece in a world, it has
-- a type (Block, Item or Player) and their own Attributes that
-- specify the aspects of a tile in the world, like the intelligence
-- level of a Player or Monster.
data Tile = Tile TT.TileType [A.Attribute]
          | Inside Tile Tile
          | Above Tile Tile
          | Tiles Tile Tile
  deriving (Show, Eq)

-- Adds an attribute to an attribute, if it is
-- of 'Inside', 'Above' or 'Tiles' type, then
-- it adds to all the dependent tiles.
addAttribute :: Tile -> [A.Attribute] -> Tile
addAttribute (Tile tt attr) attrs = Tile tt $ attr ++ attrs
addAttribute (Inside t1 t2) attrs = Inside (addAttribute t1 attrs) (addAttribute t2 attrs)
addAttribute (Above t1 t2) attrs  = Above (addAttribute t1 attrs) (addAttribute t2 attrs)
addAttribute (Tiles t1 t2) attrs  = Tiles (addAttribute t1 attrs) (addAttribute t2 attrs)

-- | Symbol of a Tile, based on its TileType
tileSymbol :: Tile -> Char
tileSymbol (Tile tileType _) = TT.symbol tileType
tileSymbol (Inside tile1 _)  = tileSymbol tile1
tileSymbol (Above tile1 _)   = tileSymbol tile1
tileSymbol (Tiles tile1 _)   = tileSymbol tile1

-- | Maps an function of type '[Attr] -> [Attr]'
-- to a Tile, and to the others dependent to it.
mapAttribute :: ([A.Attribute] -> [A.Attribute]) -> Tile -> Tile
mapAttribute fAttr (Tile tt attr) = Tile tt $ fAttr attr
mapAttribute fAttr (Above t1 t2)  = Above (mapAttribute fAttr t1) (mapAttribute fAttr t2)
mapAttribute fAttr (Inside t1 t2)  = Inside (mapAttribute fAttr t1) (mapAttribute fAttr t2)
mapAttribute fAttr (Tiles t1 t2)  = Tiles (mapAttribute fAttr t1) (mapAttribute fAttr t2)
