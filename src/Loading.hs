module Loading where

import Data.StateVar
import Data.IORef
import Data.Array
import Data.List

import World


data World = World { worldLoadedChunks :: IORef [Chunk]
                   , worldChunkQuery   :: IORef [ChunkCoord]
                   , worldState   :: IORef WorldState
                   } deriving Eq

-- | IORef for already loaded chunks.
newLoadedChunks :: IO (IORef [Chunk])
newLoadedChunks = newIORef ([] :: [Chunk])

-- | IORef for chunk coordinates to load.
newChunkQuery :: IO (IORef [ChunkCoord])
newChunkQuery = newIORef ([] :: [ChunkCoord])

-- | Get the loaded chunks and aplies a function it.
loadedChunksGet :: IORef [Chunk] -> ([Chunk] -> IO a) -> IO a
loadedChunksGet loadedChunks = (get loadedChunks >>=)

-- | Get the chunk Query and aplies a function to it.
chunkQueryGet :: IORef [ChunkCoord] -> ([ChunkCoord] -> IO a) -> IO a
chunkQueryGet chunkQuery = (get chunkQuery >>=)

-- | Remove a Chunk Coordinate from 'chunkQuery'.
removeFromQuery :: IORef [ChunkCoord] -> ChunkCoord -> IO ()
removeFromQuery chunkQuery c = chunkQuery $~ delete c

-- | Remove a Chunk from its coordinates using 'loadedChunks'.
removeCoordFromloadedChunks :: IORef [Chunk] -> ChunkCoord -> IO ()
removeCoordFromloadedChunks loadedChunks c = loadedChunks $~ deleteBy (\y x -> chunkCoord y == chunkCoord x) (array (c,c) [])

-- | Remove a Chunk using 'loadedChunks'.
removeFromloadedChunks :: IORef [Chunk] -> Chunk -> IO ()
removeFromloadedChunks loadedChunks c = loadedChunks $~ delete c

-- | Removes all loaded chunks.
clearLoadedChunks :: IORef [Chunk] -> IO ()
clearLoadedChunks loadedChunks = loadedChunks $= []

-- | Removes all chunk coordinates from chunk query.
clearChunkQuery :: IORef [ChunkCoord] -> IO ()
clearChunkQuery chunkQuery = chunkQuery $= []

-- | Adds a Chunk Coordinate to 'chunkQuery'.
addToQuery :: IORef [ChunkCoord] -> ChunkCoord -> IO ()
addToQuery chunkQuery c = chunkQuery $~ (c :)

-- | Adds a Chunk to 'loadedChunks'
addChunksToLoadedChunks :: IORef [Chunk] -> [Chunk] -> IO ()
addChunksToLoadedChunks loadedChunks chunks = loadedChunks $~ (chunks ++)

-- | Generates chunks given the coordinates and the worldState,
-- it stores the results on 'loadedChunks'.
generateChunks :: IORef [Chunk] -> [ChunkCoord] -> WorldState -> IO ()
generateChunks loadedChunks ccs ws = addChunksToLoadedChunks loadedChunks $ map (flip loadChunk ws) ccs

-- | Generates chunks given the coordinates and the worldState,
-- it stores the results on 'loadedChunks' and removes de coordinates
-- from the 'chunkQuery'.
generateChunksRemove :: IORef [ChunkCoord] -> IORef [Chunk] -> [ChunkCoord] -> WorldState -> IO ()
generateChunksRemove chunkQuery loadedChunks ccs ws = chunks >>= mapM_ (removeFromloadedChunks loadedChunks) >> chunks >>= addChunksToLoadedChunks loadedChunks
  where
    chunks = mapM (\c -> removeFromQuery chunkQuery c >> (return $ loadChunk c ws)) ccs

-- | Generate chunks from Query list.
generateFromQuery :: IORef [ChunkCoord] -> IORef [Chunk] -> WorldState ->  IO ()
generateFromQuery chunkQuery loadedChunks ws = chunkQueryGet chunkQuery $ \ccs -> generateChunksRemove chunkQuery loadedChunks ccs ws

-- | Returns loaded chunks
getLoadedChunks :: IORef [Chunk] -> IO [Chunk]
getLoadedChunks loadedChunks = loadedChunksGet loadedChunks return
