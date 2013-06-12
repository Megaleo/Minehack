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
type Chunk      = Array TileCoord T.Tile

-- A chunk is 16x16 Tiles
-- Returns the Chunk coordinates of a Tile
tileChunk :: TileCoord -> ChunkCoord
tileChunk (x, y) = (new x, new y)
  where
    new var = var `div` 16

-- The range of coordinates of a chunk, based on its coordinates
chunkRange :: ChunkCoord -> (TileCoord, TileCoord)
chunkRange (x, y) = ((min x,min y),(max x, max y))
  where
    min = (*) 16
    max = (+) 15 . min

-- Returns if it is or not in the chunk's range, based of its coordinates
isInChunkRange :: TileCoord -> ChunkCoord -> Bool
isInChunkRange tileC chunkC = tileChunk tileC == chunkC

-- Generates a Chunk (Array of Tiles with coordinates) based on seed, the tile to generate and on the chunk's coordinates
generateChunk :: Int -> T.Tile -> ChunkCoord -> Chunk
generateChunk seed tile chunkC = array rangeC $ generateChunkList seed tile chunkC
  where
    rangeC = chunkRange chunkC

-- Generates a List of Tiles and its coordinates to auxile "generateChunk"
generateChunkList :: Int -> T.Tile -> ChunkCoord -> [(TileCoord, T.Tile)]
generateChunkList seed (T.Tile tile attributes) chunkC = [(coord, (T.Tile tile attributes))| coord <- range rangeC, T.spawnCond tile seed coord]
  where
    rangeC = chunkRange chunkC

