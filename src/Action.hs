-- | An Action if a form to represent the interation of
-- a entity with the world, what it want to do, what actions
-- it want to do, like move, attack, jump, etc.
--
-- The scheme from an action to the actual Real effect in the world is:
--
--            Process Action                              Process the effect updating world to
-- Action --> by the target  ==> ([Effect], [Effect]) --> the new tiles, so when the chunks are
--            response to it         |          |         are being loaded, they load the new tiles
--                              Effects on      |
--                                Actor         |
--                                         Effects on
--                                           Target

module Action where

import Tile.TileType as TT
import World as W

-- | An action in formed by an enity that makes the action
-- , maybe a target and the type of the action (jump, run, etc.).
-- Some Actions like jump and think doesn't have a target, but others
-- like attack, drink, block have a target or somenthing to do with.
data Action = Action
    { actor :: W.CTile          -- ^ An actor entity.
    , target :: Maybe W.CTile   -- ^ Maybe a target of the action.
    , actionType :: ActionType  -- ^ The type of the action.
    } deriving (Eq, Show)

data ActionType = Move             -- ^ Move to a tile
                | Hit TT.TileType  -- ^ Hit a target with some TileType
                deriving (Eq, Show)

-- | Handles an Action to generate the effects
-- onAction :: Action -> ([Effect], [Effect])
-- onAction (Action () () ()) = undefined
