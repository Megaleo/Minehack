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
screenX = 40
screenY = 30

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
    mapM_ (putStrLn . show) (assocs newlTiles)
    maybeNewWorld <- inputAction wState $ fst $ findPlayer wState
    case maybeNewWorld of
        Nothing       -> quit
        Just newWorld -> mainLoop newWorld (findCorner (screenX,screenY) $ fst $ findPlayer newWorld) screen newlTiles

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
    let (x,y)  = corner
    let coords = [(a,b) | a <- [y..((screenX - 1)+y)], b <- [x..((screenY - 1)+x)]]
    let wState = World seed pName [(pCoord, humanTile)]
    let cTiles = map (\c -> loadTile c wState) coords
    screen <- setVideoMode (screenX * 16) (screenY * 16) 32 [SWSurface]
    mainLoop wState corner screen (array (fst $ head cTiles, fst $ last cTiles) cTiles)
