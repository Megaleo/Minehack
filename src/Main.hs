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
screenY = 40

mainASCIIloop :: WorldState -> Coord -> IO ()
mainASCIIloop ws corner = do
    printCorner ws corner (screenX,screenY)
    maybeNewWorld <- inputAction ws $ fst $ findPlayer ws
    case maybeNewWorld of
        Nothing       -> quit
        Just newWorld -> mainASCIIloop newWorld (findCorner (screenX,screenY) $ fst $ findPlayer newWorld)

mainLoop :: WorldState -> Coord -> Surface -> Array TileCoord Tile -> IO ()
mainLoop wState corner screen lTiles = do
    newlTiles <- printCornerImage wState lTiles corner tileSurface screen
    maybeNewWorld <- inputAction wState $ fst $ findPlayer wState
    case maybeNewWorld of
        Nothing       -> quit
        Just newWorld -> mainLoop newWorld (findCorner (screenX,screenY) $ fst $ findPlayer newWorld) screen newlTiles

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode (screenX * 16) (screenY * 16) 32 [SWSurface]
    putStr "Seed: "
    seed <- readLn :: IO Int
    putStr "Player's coordinates: "
    pCoord <- readLn :: IO Coord
    putStr "Name: "
    pName <- getLine
    setCaption (show seed ++ ": " ++ pName) []
    let corner = findCorner (screenX,screenY) pCoord
    let (x,y)  = corner
    let coords = [(a,b) | a <- [y..(59+y)], b <- [x..(39+x)]]
    let wState = World seed pName [(pCoord, humanTile)]
    let cTiles = map (\c -> loadTile c wState) coords
    mainLoop wState corner screen (array (fst $ head cTiles, fst $ last cTiles) cTiles)
