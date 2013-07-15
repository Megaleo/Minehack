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

import Action as A
import World

import Graphics.UI.SDL as SDL

-- | Types of recognizable Input.
data Input = MoveUp
           | MoveLeft
           | MoveRight
           | MoveDown
           deriving (Eq, Show)

-- | Transforms an Input into an action, based on
-- the player's location and the WorldState.
handleInput :: WorldState -> TileCoord -> Input -> A.Action
handleInput ws c MoveUp    = A.Action (loadTile c ws) (Just $ loadTile (c |-| (0, 1)) ws) A.Move
handleInput ws c MoveDown  = A.Action (loadTile c ws) (Just $ loadTile (c |+| (0, 1)) ws) A.Move
handleInput ws c MoveLeft  = A.Action (loadTile c ws) (Just $ loadTile (c |-| (1, 0)) ws) A.Move
handleInput ws c MoveRight = A.Action (loadTile c ws) (Just $ loadTile (c |+| (1, 0)) ws) A.Move

-- | Gets an input via 'pollEvent'.
getInput :: IO (Maybe Input)
getInput = do
    event <- SDL.pollEvent
    case event of
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_UP) _ _)     -> return $ Just MoveUp
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_LEFT) _ _)   -> return $ Just MoveLeft
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_RIGHT) _ _)  -> return $ Just MoveRight
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_DOWN) _ _)   -> return $ Just MoveDown
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_q) _ _)      -> return Nothing
        SDL.KeyDown (SDL.Keysym (SDL.SDLK_ESCAPE) _ _) -> return Nothing
        SDL.Quit                                       -> return Nothing
        _                                              -> getInput



