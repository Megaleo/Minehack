-- | An Action if a form to represent the interation of
-- a entity with the world, what it want to do, what actions
-- it want to do, like move, attack, jump, etc.
--
-- The scheme from an action to the actual Real effect in the world is:
--
--            Process Action                             Process the effect updating world to
-- Action --> by the actor  ==> ([Effect], [Effect]) --> the new tiles, so when the chunks are
--            response to it         |          |        are being loaded, they load the new tiles
--                              Effects on      |
--                                Actor         |
--                                         Effects on
--                                           Target

module Action where

import qualified Tile.TileType as TT
import qualified Tile as T
import qualified World as W
import qualified Effect as E
import qualified Block as B
import qualified Item as I

-- | An action in formed by an enity that makes the action
-- , maybe a target and the type of the action (jump, run, etc.).
-- Some Actions like jump and think doesn't have a target, but others
-- like attack, drink, block have a target or somenthing to do with.
data Action = Action
    { actor :: W.CTile          -- ^ An actor entity.
    , target :: Maybe W.CTile   -- ^ Maybe a target of the action.
    , actionType :: ActionType  -- ^ The type of the action.
    } deriving (Eq, Show)

data ActionType = Move                -- ^ Move to a tile.
                | Hit (Maybe T.Tile)  -- ^ Hit a target with maybe some Tile.
                deriving (Eq, Show)

{-
-- | If the first Tile press a key to move to the
-- second tile, this function returns the action
-- that will be made, according to the situation.
onMovement :: W.CTile -> W.CTile -> Action
onMovement actorT targetT = if T.isTileMob $ snd targetT
                            then Action actorT targetT (Hit $ weaponEquipped actorT)
                            else Action actorT targetT Move
-}

-- | Function to calculate the damage of a hit with some
-- weapon on both tiles. It returns the damage to the
-- attacker and to the target.
-- In the future, the damage will be affected by the
-- enchantment on the tile used as weapon and by
-- the skill of the attacker
onHit :: W.CTile -> W.CTile -> (Maybe T.Tile) -> (Int, Int)
onHit _ _ (Just (T.Tile tile _))  = (0, TT.damageAsWeapon tile)
onHit a t (Just (T.Above t1 t2))  = (0, (snd $ onHit a t (Just t1)) + (snd $ onHit a t (Just t2)))
onHit a t (Just (T.Inside t1 t2)) = (0, (snd $ onHit a t (Just t1)) + (snd $ onHit a t (Just t2)))
onHit a t (Just (T.Tiles t1 t2))  = (0, (snd $ onHit a t (Just t1)) + (snd $ onHit a t (Just t2)))
onHit _ _ Nothing                 = (0,1)

-- | Handles an Action to generate the effects
-- WARNING : This function will be big, very big.
onAction :: Action -> [E.Effect]
onAction (Action a (Just (c2, (T.Tile (TT.TItem I.Wood) _))) Move) = [E.Effect a (E.OccupySameSpace c2)]
onAction (Action a (Just t@(c2, (T.Tile (TT.TBlock b) _))) Move)   = if B.stayInside b
                                                                       then [E.Effect a $ E.ChangeInPositionInside c2]
                                                                       else [E.Effect a (E.Sprawl t)] ++ [E.Effect t (E.Sprawled a)]
onAction (Action a (Just t@(_, T.Tile _ _)) Move)                  = [E.Effect a (E.Sprawl t)] ++ [E.Effect t (E.Sprawled a)]
onAction (Action a (Just (c, (T.Above t1 _))) Move)                = onAction $ Action a (Just (c,t1)) Move
onAction (Action a (Just (c, (T.Inside t1 _))) Move)               = onAction $ Action a (Just (c,t1)) Move
onAction (Action a (Just (c, (T.Tiles t1 _))) Move)                = onAction $ Action a (Just (c,t1)) Move
onAction (Action a Nothing Move)                                   = [E.Effect a E.NoEffect]

onAction (Action a (Just t@(_, T.Tile _ _)) (Hit w))               = E.Effect a (E.ChangeInHP $ fst $ onHit a t w) : [E.Effect t (E.ChangeInHP $ snd $ onHit a t w)]
onAction (Action a (Just (c, (T.Above t1 _))) (Hit w))             = onAction $ Action a (Just (c,t1)) (Hit w)
onAction (Action a (Just (c, (T.Inside t1 _))) (Hit w))            = onAction $ Action a (Just (c,t1)) (Hit w)
onAction (Action a (Just (c, (T.Tiles t1 _))) (Hit w))             = onAction $ Action a (Just (c,t1)) (Hit w)
onAction (Action a Nothing (Hit _))                                = [E.Effect a (E.NoEffect)]
-- onAction (Action (coord1, tile1) (coord2, tile2) ()) = undefined
