module Tile where
-- "TileType.Flammable"   for flammable tiles
-- "TileType.Blessedness" for (bless/curse)able tiles
-- "TileType.Worth"       for valuable tiles

import qualified Attribute as A
import qualified Block as B
import qualified Item as I
-- mport qualified Player as P
import qualified Random as R

type Coord = (Int, Int)

data TileType = TBlock B.Block
              | TItem  I.Item 
              -- | TPlayer (P.Player a) 
              deriving Eq 

 -- | Tile's unique ID.
id                 :: TileType -> Int
id (TBlock B.Air)  = 0
id (TBlock B.Wood) = 1
id (TItem I.Wood)  = 2 

-- | Converts from an ID, it allows the world information
-- | to be stored with id's, and not from the Tile itself
fromId   :: Int -> TileType
fromId 0 = TBlock B.Air
fromId 1 = TBlock B.Wood
fromId 2 = TItem I.Wood
fromId _ = TBlock B.Air

-- | Tile's unique symbol.
symbol                 :: TileType -> Char
symbol (TBlock B.Air)  = '.'
symbol (TBlock B.Wood) = 'w'
symbol (TItem I.Wood)  = 'w'     

-- | Tile's unique name.
name                 :: TileType -> String
name (TBlock B.Air)  = "Air"
name (TBlock B.Wood) = "Block of Wood"
name (TItem I.Wood)  = "Item of Wood"

-- | Condition to Spawn in the world.
-- | It takes a TileType, a seed and a tile coordenate.
spawnCond                            :: TileType -> Int -> Coord -> Bool
spawnCond (TItem I.Wood) seed (x,y)  = R.rollSeed 1 100 (x*y) seed < 50
spawnCond (TBlock B.Wood) seed (x,y) = R.rollSeed 1 100 (x*y) seed < 50
spawnCond (TBlock B.Air) _ _         = True


instance Show TileType where
  showsPrec _ = showString . name

-- | Tile is the representation of each piece in a world, it has
-- a type (Block, Item or Player) and their own Attributes that
-- specify the aspects of a tile in the world, like the block
-- damage or the intelligence level of a Player.
data Tile =
  Tile TileType [A.Attribute]
  -- Above Tile Tile
  deriving (Show, Eq)