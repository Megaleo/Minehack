-- The printing.hs file does not handle the coordinates
-- as it should handle, this diagram explain:
--
--   MoveRight = (1, 0)
--   MoveLeft  = (-1,0)
--   MoveUp    = (0,-1)
--   MoveDown  = (0, 1)
--
--             (0,-)
--               |
--               |
--               |
--               |
--   (-,0)-------@-------(+,0)
--               |
--               |
--               |
--               |
--             (0,+)


module Input where

import Effect as E
import Action as A
import World

import Data.List
import Graphics.UI.SDL as SDL

-- | Types of recognizable Input.
data Input = MoveUp
           | MoveUpLeft
           | MoveUpRight
           | MoveLeft
           | MoveRight
           | MoveDown
           | MoveDownLeft
           | MoveDownRight
           | Stay
           deriving (Eq, Show)

-- | Transforms an Input into an action, based on
-- the player's location and the WorldState.
handleInput :: WorldState -> TileCoord -> Input -> A.Action
handleInput ws c MoveUp        = A.Action (loadTile c ws) (Just $ loadTile (c |-| (0, 1)) ws) A.Move
handleInput ws c MoveDown      = A.Action (loadTile c ws) (Just $ loadTile (c |+| (0, 1)) ws) A.Move
handleInput ws c MoveLeft      = A.Action (loadTile c ws) (Just $ loadTile (c |-| (1, 0)) ws) A.Move
handleInput ws c MoveRight     = A.Action (loadTile c ws) (Just $ loadTile (c |+| (1, 0)) ws) A.Move
handleInput ws c MoveUpLeft    = A.Action (loadTile c ws) (Just $ loadTile (c |+| (-1, 1)) ws) A.Move
handleInput ws c MoveUpRight   = A.Action (loadTile c ws) (Just $ loadTile (c |+| (1, 1)) ws) A.Move
handleInput ws c MoveDownRight = A.Action (loadTile c ws) (Just $ loadTile (c |+| (1,-1)) ws) A.Move
handleInput ws c MoveDownLeft  = A.Action (loadTile c ws) (Just $ loadTile (c |+| (-1,-1)) ws) A.Move
handleInput ws c Stay          = A.Action (loadTile c ws) (Just $ loadTile c ws) A.Move

-- | Gets an input via 'pollEvent'.
getInput :: IO (Maybe Input)
getInput = do
    event <- SDL.pollEvent
    case event of
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_UP) _ _)     -> return $ Just MoveUp
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP8) _ _)    -> return $ Just MoveUp
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_LEFT) _ _)   -> return $ Just MoveLeft
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP4) _ _)    -> return $ Just MoveLeft
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_RIGHT) _ _)  -> return $ Just MoveRight
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP6) _ _)    -> return $ Just MoveRight
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_DOWN) _ _)   -> return $ Just MoveDown
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP2) _ _)    -> return $ Just MoveDown
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP5) _ _)    -> return $ Just Stay
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP7) _ _)    -> return $ Just MoveUpLeft
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP9) _ _)    -> return $ Just MoveUpRight
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP1) _ _)    -> return $ Just MoveDownLeft
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_KP3) _ _)    -> return $ Just MoveDownRight
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_q) _ _)      -> return Nothing
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_ESCAPE) _ _) -> return Nothing
        SDL.Quit                                       -> return Nothing
        _                                              -> getInput
