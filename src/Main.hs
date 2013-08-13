module Main where

-- import UI.HSCurses.Curses
-- import System.IO
-- import Control.Monad

import Graphics.UI.SDL
import Data.Array
import Data.StateVar
import Data.IORef
import Data.List
import Control.Concurrent

import World
import Tile
import Printing
import Input
import Loading
-- import Item
-- import Block
-- import Player
-- import Attribute
-- import Random

screenX, screenY :: Int
screenX = 60
screenY = 45

-- | Prints the world in form of ASCII and
-- runs a input loop, like in mainLoop.
--
-- UNUSED
mainASCIIloop :: WorldState -> Coord -> IO ()
mainASCIIloop ws corner = do
    printCorner ws corner (screenX,screenY)
    maybeNewWorld <- inputAction ws $ fst $ findPlayer ws
    case maybeNewWorld of
        Nothing       -> quit
        Just newWorld -> mainASCIIloop newWorld (findCorner (screenX,screenY) $ fst $ findPlayer newWorld)

-- | Loads and prints the world  every
-- time an input event is triggered.
mainLoop :: World -> Coord -> Surface -> IO ()
mainLoop (Loading.World ioLoadedChunks ioChunkQuery ioWState) (x, y) screen = do
    wState <- get ioWState
    loadedChunks <- get ioLoadedChunks
    let chunksToLoad = chunksInScreen (x - 16, y - 16) (screenX + 32, screenY + 32)
    mapM (addToQuery ioChunkQuery) (chunksToLoad \\ map chunkCoord loadedChunks)
    let upperLeftChunk = tileChunk (x , y)
    let downRightChunk = tileChunk $ (x, y) |+| (screenX, screenY)
    let neededChunks = filter (\c -> inRange (upperLeftChunk, downRightChunk) (chunkCoord c) ) loadedChunks
    let cTilesTotal = concatMap assocs neededChunks
    let cTiles = filter (inRange ((x, y), (x, y) |+| (screenX, screenY)) . fst) cTilesTotal
    let surfaces = makeTileSurface (x, y) tileSurface (cTiles `union` wsTiles wState)
    printImage surfaces screen
    maybeNewWorld <- inputAction wState $ fst $ findPlayer wState
    case maybeNewWorld of
        Nothing       -> quit
        Just newWorld -> do
            ioWState $= newWorld
            mainLoop (Loading.World ioLoadedChunks ioChunkQuery ioWState) (findCorner (screenX,screenY) $ fst $ findPlayer newWorld) screen

-- | Main function
main :: IO ()
main = withInit [InitEverything] $ do
    putStr "Seed: "
    seed <- readLn :: IO Int
    putStr "Player's coordinates: "
    pCoord <- readLn :: IO Coord
    putStr "Name: "
    pName <- getLine
    setCaption (show seed ++ ": " ++ pName) []
    let corner = findCorner (screenX,screenY) pCoord
    let wState = World.World seed pName [(pCoord, humanTile)]
    loadedChunks <- newLoadedChunks
    chunkQuery <- newChunkQuery
    ioWState <- newIORef wState
    let localWorld = Loading.World loadedChunks chunkQuery ioWState
    forkIO $ loadingLoop localWorld 100
    screen <- setVideoMode (screenX * 16) (screenY * 16) 32 [SWSurface]
    mainLoop localWorld corner screen
