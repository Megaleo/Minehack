module Loading where

import Data.StateVar
import Data.IORef
import Data.Array
import Data.List
import Control.Concurrent

import qualified World as W


data World = World { worldLoadedChunks :: IORef [W.Chunk]
                   , worldChunkQuery   :: IORef [W.ChunkCoord]
                   , worldState        :: IORef W.WorldState
                   } deriving Eq

-- | IORef for already loaded chunks.
newLoadedChunks :: IO (IORef [W.Chunk])
newLoadedChunks = newIORef ([] :: [W.Chunk])

-- | IORef for chunk coordinates to load.
newChunkQuery :: IO (IORef [W.ChunkCoord])
newChunkQuery = newIORef ([] :: [W.ChunkCoord])

-- | Get the loaded chunks and aplies a function it.
loadedChunksGet :: IORef [W.Chunk] -> ([W.Chunk] -> IO a) -> IO a
loadedChunksGet loadedChunks = (get loadedChunks >>=)

-- | Get the chunk Query and aplies a function to it.
chunkQueryGet :: IORef [W.ChunkCoord] -> ([W.ChunkCoord] -> IO a) -> IO a
chunkQueryGet chunkQuery = (get chunkQuery >>=)

-- | Remove a Chunk Coordinate from 'chunkQuery'.
removeFromQuery :: IORef [W.ChunkCoord] -> W.ChunkCoord -> IO ()
removeFromQuery chunkQuery c = chunkQuery $~ delete c

-- | Remove a Chunk from its coordinates using 'loadedChunks'.
removeCoordFromloadedChunks :: IORef [W.Chunk] -> W.ChunkCoord -> IO ()
removeCoordFromloadedChunks loadedChunks c = loadedChunks $~ deleteBy (\y x -> W.chunkCoord y == W.chunkCoord x) (array (c,c) [])

-- | Remove a Chunk using 'loadedChunks'.
removeFromloadedChunks :: IORef [W.Chunk] -> W.Chunk -> IO ()
removeFromloadedChunks loadedChunks c = loadedChunks $~ delete c

-- | Removes all loaded chunks.
clearLoadedChunks :: IORef [W.Chunk] -> IO ()
clearLoadedChunks loadedChunks = loadedChunks $= []

-- | Removes all chunk coordinates from chunk query.
clearChunkQuery :: IORef [W.ChunkCoord] -> IO ()
clearChunkQuery chunkQuery = chunkQuery $= []

-- | Adds a Chunk Coordinate to 'chunkQuery'.
addToQuery :: IORef [W.ChunkCoord] -> W.ChunkCoord -> IO ()
addToQuery chunkQuery c = chunkQuery $~ (c :)

-- | Adds a Chunk to 'loadedChunks'
addChunksToLoadedChunks :: IORef [W.Chunk] -> [W.Chunk] -> IO ()
addChunksToLoadedChunks loadedChunks chunks = loadedChunks $~ (chunks ++)

-- | Generates chunks given the coordinates and the W.worldState,
-- it stores the results on 'loadedChunks'.
generateChunks :: IORef [W.Chunk] -> [W.ChunkCoord] -> W.WorldState -> IO ()
generateChunks loadedChunks ccs ws = addChunksToLoadedChunks loadedChunks $ map (flip W.loadChunk ws) ccs

-- | Generates chunks given the coordinates and the W.worldState,
-- it stores the results on 'loadedChunks' and removes de coordinates
-- from the 'chunkQuery'.
generateChunksRemove :: IORef [W.ChunkCoord] -> IORef [W.Chunk] -> [W.ChunkCoord] -> W.WorldState -> IO ()
generateChunksRemove chunkQuery loadedChunks ccs ws = chunks >>= mapM_ (removeFromloadedChunks loadedChunks) >> chunks >>= addChunksToLoadedChunks loadedChunks
  where
    chunks = mapM (\c -> removeFromQuery chunkQuery c >> (return $ W.loadChunk c ws)) ccs

-- | Generate chunks from Query list.
generateFromQuery :: IORef [W.ChunkCoord] -> IORef [W.Chunk] -> W.WorldState ->  IO ()
generateFromQuery chunkQuery loadedChunks ws = chunkQueryGet chunkQuery $ \ccs -> generateChunksRemove chunkQuery loadedChunks ccs ws

-- | Returns loaded chunks
getLoadedChunks :: IORef [W.Chunk] -> IO [W.Chunk]
getLoadedChunks loadedChunks = loadedChunksGet loadedChunks return

-- | Loads and generates constantly from a World State.
loadingLoop :: World -> Int -> IO ()
loadingLoop world@(World loadedChunks chunkQuery ioWorldState) delay = do
    query <- get chunkQuery
    if null query
        then threadDelay delay >> loadingLoop world delay
        else do
            get ioWorldState >>= generateFromQuery chunkQuery loadedChunks
            loadingLoop world delay
