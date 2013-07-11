module Printing where

import System.Console.ANSI (clearScreen)

import qualified Tile as T
import qualified World as W

screenWidth :: Int
screenWidth = 60

screenHeight :: Int
screenHeight = 30

-- | Finds the player when the world has
-- only it in the modified tiles. If it doesn't
-- find any player tile, then it returns a
-- default 'humanTile' in (0,0).
findPlayer :: W.WorldState -> W.CTile
findPlayer (W.World s n (p@(_,t):ps)) = if T.isTilePlayer t then p else findPlayer $ W.World s n ps
findPlayer (W.World _ _ [])           = ((0, 0), T.humanTile)

-- | Returns the top left corner from the size
-- of the screen and the player's coordinates.
findCorner :: W.Coord -> W.Coord -> W.Coord
findCorner (sx,sy) (px,py) = (py - (sx `div` 2), px - (sy `div` 2))

-- | Prints a World based on the upper left coordinate.
printCorner :: W.WorldState -> W.Coord -> IO ()
printCorner ws (x, y) = do
    clearScreen
    putStrLn $ replicate (screenWidth + 2) '#'
    putStr $ sep16 $ map (T.tileSymbol . snd) $ map (\c -> W.loadTile c ws) coords
    putStrLn $ replicate (screenWidth + 2) '#'
    where
        coords    = [(a,b) | a <- [y..(height+y)], b <- [x..(width+x)]]
        height    = screenHeight - 1
        width     = screenWidth - 1
        sep16 []  = []
        sep16 str = "#" ++ (take 60 str) ++ "#\n" ++ (sep16 $ drop 60 str)
