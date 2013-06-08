module World where
--
-- The way to handle "infinite" world elements is, in the nutshell, load 16x16 Tiles chunks with 
-- random generation (seed) and verify if any of those have modified tiles.
--
-- This module will care of these world stuff, loading chunks mainly.

import System.IO
import Control.Monad
import Data.Array

import qualified Tile as T

-- type synonyms
type Coord      = (Int, Int)
type TileCoord  = Coord
type ChunkCoord = Coord

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

