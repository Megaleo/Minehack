module Main where

-- import UI.HSCurses.Curses
-- import System.IO
-- import Control.Monad

import Graphics.UI.SDL

import World
import Tile
import Printing
import Input
-- import Item
-- import Block
-- import Player
-- import Attribute
-- import Random

mainLoop :: WorldState -> Coord -> Surface -> IO ()
mainLoop wState corner screen = do
    printCornerImage wState corner tileSurface screen
    maybeNewWorld <- inputAction wState $ fst $ findPlayer wState
    case maybeNewWorld of
        Nothing       -> quit
        Just newWorld -> mainLoop newWorld (findCorner (60,60) $ fst $ findPlayer wState) screen

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode 960 960 32 [SWSurface]
    putStr "Seed: "
    seed <- readLn :: IO Int
    putStr "Player's coordinates: "
    pCoord <- readLn :: IO Coord
    putStr "Name: "
    pName <- getLine
    setCaption (show seed ++ ": " ++ pName) []
    let corner = findCorner (60,60) pCoord
    mainLoop (World seed pName [(pCoord, humanTile)]) corner screen
