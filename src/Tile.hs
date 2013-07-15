module Tile where

import qualified Tile.TileType as TT
import qualified Attribute as A
import qualified Item as I
import qualified Block as B
import qualified Entity as E
import qualified Entity.Player as EP

-- | Tile is the representation of each piece in a world, it has
-- a type (Block, Item or Player) and their own Attributes that
-- specify the aspects of a tile in the world, like the intelligence
-- level of a Player or Monster.
data Tile = Tile TT.TileType [A.Attribute]
          | Inside Tile Tile
          | Above Tile Tile
          | Tiles Tile Tile
  deriving (Show, Eq, Read)

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
tileSymbol tile              = tileSymbol $ mainTile tile

-- | Maps an function of type '[Attr] -> [Attr]'
-- to a Tile, and to the others dependent to it.
mapAttribute :: ([A.Attribute] -> [A.Attribute]) -> Tile -> Tile
mapAttribute fAttr (Tile tt attr) = Tile tt $ fAttr attr
mapAttribute fAttr (Above t1 t2)  = Above (mapAttribute fAttr t1) (mapAttribute fAttr t2)
mapAttribute fAttr (Inside t1 t2)  = Inside (mapAttribute fAttr t1) (mapAttribute fAttr t2)
mapAttribute fAttr (Tiles t1 t2)  = Tiles (mapAttribute fAttr t1) (mapAttribute fAttr t2)

noAttributes :: Tile -> Tile
noAttributes = mapAttribute (\_ -> [])

-- | Tile made of a block of wood, for testing purposes.
blockWoodTile :: Tile
blockWoodTile = Tile (TT.TBlock B.Wood) []

-- | Tile made of a item of wood, for testing purposes.
itemWoodTile :: Tile
itemWoodTile = Tile (TT.TItem I.Wood) []

-- | Tile made of air, for testing purposes.
airTile :: Tile
airTile = Tile (TT.TBlock B.Air) []

-- | Tile made of an human, for testing purposes.
humanTile :: Tile
humanTile = Tile (TT.TEntity (E.EPlayer (EP.Human))) []

-- | Returns the tile inside of air.
withAir :: Tile -> Tile
withAir tile = Inside tile airTile

-- | Returns the main tile of any tile.
-- If it is a 'Inside', 'Above' or 'Tiles'
-- tile, then it returns the first tile, the principal.
mainTile :: Tile -> Tile
mainTile t@(Tile _ _)  = t
mainTile (Inside t1 _) = t1
mainTile (Above t1 _)  = t1
mainTile (Tiles t1 _)  = t1

-- | Returns the TileType of the main Tile.
typeOfTile :: Tile -> TT.TileType
typeOfTile (Tile tt _) = tt
typeOfTile tile        = typeOfTile $ mainTile tile

-- | Returns if the main Tile is an Entity.
isTileEntity :: Tile -> Bool
isTileEntity (Tile tt _) = TT.isOnlyEntity tt
isTileEntity tile        = isTileEntity $ mainTile tile

-- | Returns if the main Tile is a Mob.
isTileMob :: Tile -> Bool
isTileMob (Tile tt _) = TT.isOnlyMob tt
isTileMob tile        = isTileMob $ mainTile tile

-- | Returns if the main Tile is a Player.
isTilePlayer :: Tile -> Bool
isTilePlayer (Tile tt _) = TT.isOnlyPlayer tt
isTilePlayer tile        = isTilePlayer $ mainTile tile

-- | Returns if the main Tile is a Block.
isTileBlock :: Tile -> Bool
isTileBlock (Tile tt _) = TT.isOnlyBlock tt
isTileBlock tile        = isTileBlock $ mainTile tile

-- | Returns if the main Tile is an Item.
isTileItem :: Tile -> Bool
isTileItem (Tile tt _) = TT.isOnlyItem tt
isTileItem tile        = isTileItem $ mainTile tile
