-- | The way to handle "infinite" world elements is, in the
-- nutshell, load 16x16 Tiles chunks with random generation
-- (seed) and verify if any of those have modified tiles.
--
-- This module will care of these world stuff, loading chunks
-- mainly.
--
-- TODO
--
-- @ Add suport for biomes

module World where

import Data.Array

import Numeric.Noise
import Numeric.Noise.Perlin

import qualified Tile as T

-- | Type synonyms
type Coord      = (Int, Int)
type TileCoord  = Coord
type ChunkCoord = Coord
-- | Chunk = Array of Tile's id (TileType and Attributes` id) and its coordenates 
type Chunk      = Array TileCoord T.Tile

-- | A chunk is 16x16 'Tile's.
-- Returns the 'Chunk' coordinates of a 'Tile'.
tileChunk :: TileCoord -> ChunkCoord
tileChunk (x, y) = (new x, new y)
  where
    new var = var `div` 16

-- | The range of coordinates of a 'Chunk', based on its coordinates.
chunkRange :: ChunkCoord -> (TileCoord, TileCoord)
chunkRange (x, y) = ((min_ x, min_ y),(max_ x, max_ y))
  where
    min_ = (*) 16
    max_ = (+) 15 . min_

-- | Returns if it is or not in the chunk's range, based of its coordinates.
isInChunkRange :: TileCoord -> ChunkCoord -> Bool
isInChunkRange tileC chunkC = tileChunk tileC == chunkC

-- | Generates a 'Chunk' ('Array' of 'Tile's with coordinates)
-- based on seed, the tile to generate and on the chunk's
-- coordinates.
generateChunk :: Int -> T.Tile -> ChunkCoord -> Chunk
generateChunk seed tile chunkC = array rangeC $ generateChunkList seed tile chunkC
  where
    rangeC = chunkRange chunkC

-- | Generates a list of TileType and Attributes, with its coordenates 
-- 'generateChunk'.
generateChunkList :: Int -> T.Tile -> ChunkCoord -> [(TileCoord, T.Tile)]
generateChunkList seed (T.Tile tile attributes) chunkC = [(coord, T.Tile tile attributes) | coord <- range rangeC, T.spawnCond tile seed coord]
  where
    rangeC = chunkRange chunkC

-- | A way to specify what should be returned when a value in a map of Perlin 
-- satisfies a boundary.
data PerlinBoundary a = PBoundary Int Int a

-- | Data type for gather all the information necessarily to make Perlin based chunk.
data PerlinChunk = PChunk Double ChunkCoord [PerlinBoundary T.Tile]

-- | Generates a 2D map with type Double from a Perlin. 
twoD             :: Double -> Double -> Perlin -> Double
twoD x y perlin_ = noiseValue perlin_ (x,y,1)

-- | Multiplies by a number all the values of a 16x16 2D map of Perlin generated by
-- "twoD" in some chunk coordenates and rounds all the results.
chunkMap                          :: Double -> Perlin -> ChunkCoord -> [Int]
chunkMap multiplier perlin_ coord = [round $ multiplier * (twoD (toEnum x) (toEnum y) perlin_) | (x, y) <- range $ chunkRange coord]