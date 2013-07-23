module Main where

-- import UI.HSCurses.Curses
-- import System.IO
-- import Control.Monad

import Graphics.UI.SDL
import Data.Array

import World
import Tile
import Printing
import Input
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
mainASCIIloop :: WorldState -> Coord -> IO ()
mainASCIIloop ws corner = do
    printCorner ws corner (screenX,screenY)
    maybeNewWorld <- inputAction ws $ fst $ findPlayer ws
    case maybeNewWorld of
        Nothing       -> quit
        Just newWorld -> mainASCIIloop newWorld (findCorner (screenX,screenY) $ fst $ findPlayer newWorld)

-- | Loads and prints the world  every
-- time an input event is triggered.
mainLoop :: WorldState -> Coord -> Surface -> IO ()
mainLoop wState (x, y) screen = do
    let height  = screenY - 1
    let width   = screenX - 1
    let coords  = [(b,a) | a <- [y..(height+y)], b <- [x..(width+x)]] -- ^ All the coordinates
    let cTiles = map (Prelude.flip loadTile wState) coords
    printImage (array (fst $ head cTiles, fst $ last cTiles) cTiles) tileSurface screen
    maybeNewWorld <- inputAction wState $ fst $ findPlayer wState
    case maybeNewWorld of
        Nothing       -> quit
        Just newWorld -> mainLoop newWorld (findCorner (screenX,screenY) $ fst $ findPlayer newWorld) screen

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
    let wState = World seed pName [(pCoord, humanTile)]
    screen <- setVideoMode (screenX * 16) (screenY * 16) 32 [SWSurface]
    mainLoop wState corner screen
