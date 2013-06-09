module World where
--
-- The way to handle "infinite" world elements is, in the nutshell, load 16x16 Tiles chunks with 
-- random generation (seed) and verify if any of those have modified tiles.
--
-- This module will care of these world stuff, loading chunks mainly.

import System.IO
import Control.Monad
import Data.Array 
import Data.Ix

import qualified Tile as T
import qualified Attribute as A

-- type synonyms
type Coord      = (Int, Int)
type TileCoord  = Coord
type ChunkCoord = Coord
data Chunk where
  Chunk :: (T.TileType t, A.Attribute a) => Array TileCoord (T.Tile t [a]) -> Chunk

-- A chunk is 16x16 Tiles
tileChunk :: TileCoord -> ChunkCoord
tileChunk (x, y) = (new x, new y)
  where
    new var = floor $ (toEnum var) / 16 

chunkRange :: ChunkCoord -> (TileCoord, TileCoord)
chunkRange (x, y) = ((min x,min y),(max x, max y))
  where
    min = (*) 16
    max = (+) 15 . min 
    
isInChunkRange :: TileCoord -> ChunkCoord -> Bool
isInChunkRange tileC chunkC = tileChunk tileC == chunkC   

generateChunk :: (T.TileType t, A.Attribute a) => Int -> T.Tile t [a] -> ChunkCoord -> Chunk
generateChunk seed (T.Tile tile attributes) chunkC = Chunk $ array rangeC [(coord,(T.Tile tile attributes))| coord <- range rangeC, T.spawnCond tile seed coord]
  where
    rangeC = chunkRange chunkC
