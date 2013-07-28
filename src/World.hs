-- | The way to handle "infinite" world elements is, in the
-- nutshell, load 16x16 Tiles chunks with random generation
-- (seed) and verify if any of those have modified tiles.
--
-- This module will care of these world stuff, loading chunks
-- mainly.
--
-- TODO
--
-- @ Add suport for more complex biomes.
-- @ Add transition biomes.

module World where

import Data.Array
import Data.List
import Data.IORef
import Data.StateVar

import Numeric.Noise
import Numeric.Noise.Perlin

import qualified Tile as T
import qualified Tile.TileType as TT
import qualified Block as B
import qualified Item as I

-- | Type synonyms.
type Coord      = (Int, Int)
type TileCoord  = Coord
type ChunkCoord = Coord
-- | Chunk = Array of Tiles and its coordinates.
type Chunk      = Array TileCoord T.Tile
-- | A tuple of a tile and its coordinates.
type CTile      = (TileCoord, T.Tile)

(|+|) :: Coord -> Coord -> Coord
(x1,y1) |+| (x2,y2) = (x1 + x2, y1 + y2)

(|-|) :: Coord -> Coord -> Coord
(x1,y1) |-| (x2,y2) = (x1 - x2, y1 - y2)

(|*|) :: Coord -> Int -> Coord
(x1,y1) |*| m = (x1 * m, y1 * m)

(|/|) :: Coord -> Int -> Coord
(x1,y1) |/| d = (x1 `div` d, y1 `div` d)

-- Returns the Chunk coordinates of a Tile.
tileChunk :: TileCoord -> ChunkCoord
tileChunk (x, y) = (new x, new y)
  where
    new var = var `div` 16

chunkCoord:: Chunk -> ChunkCoord
chunkCoord = tileChunk . fst . bounds

-- | The range of tiles coordinates of a Chunk.
chunkRange :: ChunkCoord -> (TileCoord, TileCoord)
chunkRange (x, y) = ((min_ x, min_ y),(max_ x, max_ y))
  where
    min_ = (*) 16
    max_ = (+) 15 . min_

-- | Returns if a tile coordenate is in the chunk's range.
isInChunkRange :: TileCoord -> ChunkCoord -> Bool
isInChunkRange tileC chunkC = tileChunk tileC == chunkC

-- | Generates a 2D map with type Double from a Perlin.
twoD :: Double -> Double -> Perlin -> Double
twoD x y perlin_ = noiseValue perlin_ (x,y,1)

-- | Multiplies by a number all the values of a 16x16 2D map of Perlin generated by
-- "twoD" in some chunk coordinates and rounds all the results.
-- Note: Always perlin noise by itself returns values from -1 to 1.
chunkPMap :: Double -> Perlin -> ChunkCoord -> [Int]
chunkPMap mult perlin_ coord = [round $ mult * (twoD (toEnum x) (toEnum y) perlin_) | (x, y) <- range $ chunkRange coord]

-- | Generates a list of tiles based on the multiplier, perlin and some chunk coordinates
-- for chunkPMap and a function that picks a values in the perlin map and returns a
-- tile. mapping it, genPerlinChunk generates a list of tiles.
genPrimitiveChunk :: Double -> Perlin -> ChunkCoord -> (Int -> T.Tile) -> [T.Tile]
genPrimitiveChunk mult perlin_ cCoord pFunc = map pFunc $ chunkPMap mult perlin_ cCoord

-- | Uses the same technic as in genPrimitiveChunk
-- but it returns a Chunk (Array of Tiles).
genChunk :: Double -> Perlin -> ChunkCoord -> (Int -> T.Tile) -> Chunk
genChunk mult perlin_ cCoord pFunc = array (chunkRange cCoord) $ zip (range $ chunkRange cCoord) (genPrimitiveChunk mult perlin_ cCoord pFunc)

-- | The standart multiplication factor for
-- CPM and biome generation (CPM stands for
-- "Chunk Perlin Map").
stdMult :: Double
stdMult = 100.0

-- | A simple biome is a function that returns a tile
-- given a value on the Chunk Perlin Map, some arguments
-- for Perlin creation and a multiplier.
data SimpleBiome = SBiome { cpmFunc :: Int -> T.Tile     -- ^ Takes a value on the CPM
                          , perlinArgs :: Int -> Perlin  -- ^ Takes a Seed by argument
                          , multiplier :: Double         -- ^ Multiplier for all values
                          }

-- | Generates a list of tiles by applying genPrimitiveChunk
-- handling data from SimpleBiome.
genPrimitiveSBiomeChunk :: Int -> ChunkCoord -> SimpleBiome -> [T.Tile]
genPrimitiveSBiomeChunk seed cCoord sBiome = genPrimitiveChunk mult perlin_ cCoord cpmF
    where
        mult    = multiplier sBiome
        perlin_ = (perlinArgs sBiome) seed
        cpmF    = cpmFunc sBiome

-- | Uses the same technic as in genPrimitiveSBiomeChunk
-- but it returns a Chunk (Array of Tiles).
genSBiomeChunk :: Int -> ChunkCoord -> SimpleBiome -> Chunk
genSBiomeChunk seed cCoord sBiome = array (chunkRange cCoord) $ zip (range $ chunkRange cCoord) (genPrimitiveSBiomeChunk seed cCoord sBiome)

-- | A Simple Forest made out of "Block of Wood",
-- "Item of Wood" and "Air".
simpleForest :: SimpleBiome
simpleForest = SBiome cpmMap perlin_ stdMult
    where
        cpmMap value
          | value < 0  = T.Tile (TT.TBlock B.Air) []
          | value < 10 = T.withAir $ T.Tile (TT.TItem I.Wood) []
          | otherwise  = T.withAir $ T.Tile (TT.TBlock B.Wood) []
        perlin_ seed = perlin seed 5 0.3 0.1

-- | A very simple plains biome made
-- out of "Blobk of Wood", "Item of Wood"
-- and "Air"
simplePlain :: SimpleBiome
simplePlain = SBiome cpmMap perlin_ stdMult
    where
        cpmMap value
          | value < 60 = T.Tile (TT.TBlock B.Air) []
          | value < 80 = T.withAir $ T.Tile (TT.TItem I.Wood) []
          | otherwise  = T.withAir $ T.Tile (TT.TBlock B.Wood) []
        perlin_ seed = perlin seed 5 0.3 0.1

simpleOcean :: SimpleBiome
simpleOcean = SBiome cpmMap perlin_ stdMult
    where
        cpmMap value
          | value < 50 = T.Tile (TT.TBlock B.DeepWater) []
          | value < 60 = T.Tile (TT.TBlock B.Water) []
          | value < 85 = T.Tile (TT.TBlock B.Sand) []
          | otherwise  = T.Tile (TT.TBlock B.Air) []
        perlin_ seed = perlin seed 5 0.2 0.1

-- | A WorldState is made of a Seed, a Name of the World
-- and a list of modified tiles and its coordinates.
data WorldState = World Seed String [CTile]
                deriving (Eq, Show, Read)

-- | Loads a Chunk from the chunk coordinates, a simple biome for
-- the chunk and the world state.
loadSBiomeChunk :: ChunkCoord -> SimpleBiome -> WorldState -> Chunk
loadSBiomeChunk cCoord sBiome (World seed _ tiles) = genSBiomeChunk seed cCoord sBiome // filter (\(x,_) -> isInChunkRange x cCoord) tiles

-- | A Simple Biome Map is like a Simple Biome, but
-- it returns a Simple Biome itself frmm a value in
-- a Perlin Map.
data SimpleBiomeMap = SBiomeM { cpmFuncM :: Int -> SimpleBiome  -- ^ Takes a value on the CPM
                              , perlinArgsM :: Int -> Perlin    -- ^ Takes a Seed by argument
                              , multiplierM :: Double           -- ^ Multiplier for all values
                              }

-- | A standart biome map.
stdBiomeMap :: SimpleBiomeMap
stdBiomeMap = SBiomeM cpmFM perlin_M stdMult
    where
        cpmFM value
          | value < 0  = simpleForest
          | value < 50 = simplePlain
          | otherwise  = simpleOcean
        perlin_M seed = perlin seed 5 0.01 0.001

-- | Loads a chunk from the chunk coordinates, a Simple Biome
-- Map and a World state.
loadSBiomeMChunk :: ChunkCoord -> SimpleBiomeMap -> WorldState -> Chunk
loadSBiomeMChunk (x,y) sBiomeM wState@(World seed _ _) = loadSBiomeChunk (x,y) sBiome wState
    where
        sBiome  = (cpmFuncM sBiomeM) (fromEnum value)
        value   = (multiplierM sBiomeM) * (twoD (toEnum x) (toEnum y) perlin_)
        perlin_ = (perlinArgsM sBiomeM) seed

-- | Loads a chunk using 'stdBiomeMap' in the
-- function 'loadSBiomeMChunk'.
loadChunk :: ChunkCoord -> WorldState -> Chunk
loadChunk cCoord wState = loadSBiomeMChunk cCoord stdBiomeMap wState

-- | Loads a Tile by the World State.
loadTile :: TileCoord -> WorldState -> CTile
loadTile c (World seed _ tiles) = case lookup c tiles of
                                      Just t  -> (c,t)
                                      Nothing -> (c, (cpmFunc ((cpmFuncM stdBiomeMap) value1) value2))
                                                  where
                                                      sBiome = (cpmFuncM stdBiomeMap) value1
                                                      value1 = fromEnum $ (multiplierM stdBiomeMap) * (twoD (toEnum $ fst $ tileChunk c) (toEnum $ snd $ tileChunk c) ((perlinArgsM stdBiomeMap) seed))
                                                      value2 = fromEnum $ (multiplier sBiome) * (twoD (toEnum $ fst c) (toEnum $ snd c) ((perlinArgs sBiome) seed))

-- | Verifies if an CTile exist in an WorldState.
existInTiles :: CTile -> WorldState -> Bool
existInTiles ctile (World _ _ tiles) = ctile `elem` tiles

-- | Returns the CTile if it
-- exists in the WorldState.
returnTile :: CTile -> WorldState -> Maybe CTile
returnTile ctile ws = if existInTiles ctile ws
                          then Just ctile
                          else Nothing

-- | If a tile is already modified in the World State, then
-- it replaces with the new tile, if not, then it just put
-- the new tile in the World State.
changeTile :: WorldState -> CTile -> WorldState
changeTile (World s str tiles) (c, tile) = case lookup c tiles of
                                               Just t  -> World s str $ (c,tile) : delete (c,t) tiles
                                               Nothing -> World s str $ (c,tile) : tiles

-- | Deletes an CTile from a list.
-- If it is of type 'Tiles', 'Above'
-- or 'Inside', then it deletes the
-- first tile, remaining the second.
deleteTile :: CTile -> [CTile] -> [CTile]
deleteTile (c, t) tiles = case lookup c tiles of
                              Just (T.Above _ t2)  -> (c, t2) : delete (c, t) tiles
                              Just (T.Inside _ t2) -> (c, t2) : delete (c, t) tiles
                              Just (T.Tiles _ t2)  -> (c, t2) : delete (c, t) tiles
                              Just _               -> delete (c, t) tiles
                              Nothing              -> tiles

wsTiles :: WorldState -> [CTile]
wsTiles (World _ _ tiles) = tiles

-------------------------
-- Automatic loading
-------------------------

-- | IORef for already loaded chunks.
loadedChunks :: IO (IORef [Chunk])
loadedChunks = newIORef ([] :: [Chunk])

-- | IORef for chunk coordinates to load.
chunkQuery :: IO (IORef [ChunkCoord])
chunkQuery = newIORef ([] :: [ChunkCoord])

-- | Do a function using loadedChunks IORef.
loadedChunksDo :: (IORef [Chunk] -> IO a) -> IO a
loadedChunksDo = (loadedChunks >>=)

-- | Get the loaded chunks and aplies a function it.
loadedChunksGet :: ([Chunk] -> IO a) -> IO a
loadedChunksGet = (loadedChunks >>= get >>=)

-- | Do a function using chunk query IORef.
chunkQueryDo :: (IORef [ChunkCoord] -> IO a) -> IO a
chunkQueryDo = (chunkQuery >>=)

-- | Get the chunk Query and aplies a function to it.
chunkQueryGet :: ([ChunkCoord] -> IO a) -> IO a
chunkQueryGet = (chunkQuery >>= get >>=)

-- | Remove a Chunk Coordinate from 'chunkQuery'.
removeFromQuery :: ChunkCoord -> IO ()
removeFromQuery c = chunkQueryDo ($~ delete c)

-- | Remove a Chunk from its coordinates using 'loadedChunks'.
removeCoordFromloadedChunks :: ChunkCoord -> IO ()
removeCoordFromloadedChunks c = loadedChunksDo ($~ deleteBy (\y x -> chunkCoord y == chunkCoord x) (array (c,c) []))

-- | Remove a Chunk using 'loadedChunks'.
removeFromloadedChunks :: Chunk -> IO ()
removeFromloadedChunks c = loadedChunksDo ($~ delete c)

-- | Removes all loaded chunks.
clearLoadedChunks :: IO ()
clearLoadedChunks = loadedChunksDo ($= [])

-- | Removes all chunk coordinates from chunk query.
clearChunkQuery :: IO ()
clearChunkQuery = chunkQueryDo ($= [])

-- | Adds a Chunk Coordinate to 'chunkQuery'.
addToQuery :: ChunkCoord -> IO ()
addToQuery c = chunkQuery >>= ($~ (c :))

-- | Adds a Chunk to 'loadedChunks'
addChunksToLoadedChunks :: [Chunk] -> IO ()
addChunksToLoadedChunks chunks = loadedChunksDo ($~ (chunks ++))

-- | Generates chunks given the coordinates and the worldState,
-- it stores the results on 'loadedChunks'.
generateChunks :: [ChunkCoord] -> WorldState -> IO ()
generateChunks ccs ws = addChunksToLoadedChunks $ map (flip loadChunk ws) ccs

-- | Generates chunks given the coordinates and the worldState,
-- it stores the results on 'loadedChunks' and removes de coordinates
-- from the 'chunkQuery'.
generateChunksRemove :: [ChunkCoord] -> WorldState -> IO ()
generateChunksRemove ccs ws = chunks >>= mapM_ removeFromloadedChunks >> chunks >>= addChunksToLoadedChunks
  where
    chunks = mapM (\c -> removeFromQuery c >> (return $ loadChunk c ws)) ccs

-- | Generate chunks from Query list.
generateFromQuery :: WorldState ->  IO ()
generateFromQuery ws = chunkQueryGet $ flip generateChunksRemove ws
