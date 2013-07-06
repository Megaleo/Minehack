-- | The way to handle "infinite" world elements is, in the
-- nutshell, load 16x16 Tiles chunks with random generation
-- (seed) and verify if any of those have modified tiles.
--
-- This module will care of these world stuff, loading chunks
-- mainly.
--
-- TODO
--
-- @ Add suport for more complex biomes

module World where

import Data.Array

import Numeric.Noise
import Numeric.Noise.Perlin

import qualified Tile as T
import qualified Block as B
import qualified Item as I

-- | Type synonyms
type Coord      = (Int, Int)
type TileCoord  = Coord
type ChunkCoord = Coord
-- | Chunk = Array of Tiles and its coordenates
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

-- | Generates a 2D map with type Double from a Perlin.
twoD             :: Double -> Double -> Perlin -> Double
twoD x y perlin_ = noiseValue perlin_ (x,y,1)

-- | Multiplies by a number all the values of a 16x16 2D map of Perlin generated by
-- "twoD" in some chunk coordenates and rounds all the results.
-- Always perlin noise by itself returns values from -1 to 1, so when
chunkPMap                    :: Double -> Perlin -> ChunkCoord -> [Int]
chunkPMap mult perlin_ coord = [round $ mult * (twoD (toEnum x) (toEnum y) perlin_) | (x, y) <- range $ chunkRange coord]

-- | Generates a chunk based on the multiplier, pelin and chunk coordenates for
-- chunkPMap and a function that picks a values in the perlin map and returns a
-- tile. mapping it, genPerlinChunk generates a chunk
genChunk                           :: Double -> Perlin -> ChunkCoord -> (Int -> T.Tile) -> [T.Tile]
genChunk mult perlin_ cCoord pFunc = map pFunc $ chunkPMap mult perlin_ cCoord

-- | The standart multiplication factor for
-- CPM and biome generation (CPM stands for
-- "Chunk Perlin Map").
stdMult :: Double
stdMult = 100.0

-- | A simple biome is a function that returns a tile
-- given a value on the Chunk Perlin Map, some arguments
-- for Perlin creation and a multiplier.
data SimpleBiome = SBiome { cpmFunc :: Int -> T.Tile
                          , perlinArgs :: Int -> Perlin -- Takes a Seed by argument
                          , multiplier :: Double }

genSBiomeChunk                    :: Int -> ChunkCoord -> SimpleBiome -> [T.Tile]
genSBiomeChunk seed cCoord sBiome = genChunk mult perlin_ cCoord cpmF
    where
        mult    = multiplier sBiome
        perlin_ = (perlinArgs sBiome) seed
        cpmF    = cpmFunc sBiome

simpleForest :: SimpleBiome
simpleForest = SBiome cpmMap perlin_ stdMult
    where
        cpmMap value = if value < 0
                       then T.Tile (T.TBlock B.Air) []
                       else if value < 10
                       then T.Tile (T.TItem I.Wood) []
                       else T.Tile (T.TBlock B.Wood) []

        perlin_ seed = perlin seed 5 0.07 0.05