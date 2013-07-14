module Printing where

import System.Console.ANSI (clearScreen)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi
import Control.Monad

import qualified Tile as T
import qualified World as W
import qualified Tile.TileType as TT
import qualified Block as B
import qualified Item as I
-- import qualified Entity as E
-- import qualified Entity.Player as EP

{- TEXTUAL PART -}

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
findCorner (sx,sy) (px,py) = (px - (sx `div` 2), py - (sy `div` 2))

-- | Prints a World based on the upper left coordinate.
printCorner :: W.WorldState -> W.Coord -> W.Coord -> IO ()
printCorner ws (x, y) (sWidth, sHeight) = do
    clearScreen
    putStrLn $ replicate (sWidth + 2) '#'
    putStr $ sep16 $ map (T.tileSymbol . snd) $ map (\c -> W.loadTile c ws) coords
    putStrLn $ replicate (sWidth + 2) '#'
    where
        coords    = [(a,b) | a <- [y..(height+y)], b <- [x..(width+x)]]
        height    = sHeight - 1
        width     = sWidth - 1
        sep16 []  = []
        sep16 str = "#" ++ (take 60 str) ++ "#\n" ++ (sep16 $ drop 60 str)


{- GRAPHICAL PART -}

-- | Utility function to blit surfaces.
applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

-- | Returns the surface of a given Tile
tileSurface :: T.Tile -> IO SDL.Surface
tileSurface (T.Tile (TT.TBlock B.Wood) _) = SDLi.load "textures/log_oak.png"
tileSurface (T.Tile (TT.TItem I.Wood) _)  = SDLi.load "textures/planks_oak.png"
tileSurface (T.Tile (TT.TBlock B.Air) _)  = SDLi.load "textures/air.png"
tileSurface (T.Tile (TT.TEntity _) _)     = SDLi.load "textures/human.png"
tileSurface tile                          = tileSurface $ T.mainTile tile

-- | Prints the image of the world given the world state, the upper left
-- coordinates of the screen and the surface of the screen.
printCornerImage :: W.WorldState -> W.Coord -> SDL.Surface -> IO ()
printCornerImage ws (x, y) screen = SDL.withInit [SDL.InitEverything] $ do
    Rect _ _ sTWidth sTHeight <- getClipRect screen
    let sWidth  = sTWidth `div` 16 -- ^ divides the width by 16 to get it in terms of tiles
    let sHeight = sTHeight `div` 16 -- ^ divides the height by 16 to get it in terms of tiles
    let height  = sHeight - 1
    let width   = sWidth - 1
    let coords  = [(a,b) | a <- [y..(height+y)], b <- [x..(width+x)]] -- ^ All the coordinates
    let cTiles  = map (\c -> W.loadTile c ws) coords -- ^ loads all the tiles that will be displayed
    let cTilesWOffset = map (coordDiff (x,y)) cTiles -- ^ Tiles with its coordinates as offsets to the upper left corner.
    surfaces <- mapM (tileSurface . snd) cTilesWOffset -- ^ The surfaces of all tiles
    mapM_  (showSTile screen) $ zip (map fst cTilesWOffset) surfaces -- ^ Maps the showSTile to the surfaces with its offsets
    SDL.flip screen -- ^ Flips the screen
        where
            -- I know that mix 'let' and 'where' expressions is not good. -.- --
            -- With the surface, it multiplies all the offsets by a quality factor and
            -- uses 'applySurface' to blit to the source surface.
            showSTile src ((a,b),surface) = applySurface (a*16) (b*16) surface src Nothing
            coordDiff (x1,y1) ((x2,y2),tile) = (((x2-x1), (y2-y1)),tile) -- ^ The difference betwen two coordinates

-- | Loop that quits itself if an 'Exit'
-- signalis sent, when the user closes the window.
quitLoop :: IO ()
quitLoop = do
    quitSignal <- whileEvents
    unless quitSignal quitLoop
        where
            whileEvents = do
                event <- pollEvent
                case event of
                    Quit    -> return True
                    NoEvent -> return False
                    _       -> whileEvents

