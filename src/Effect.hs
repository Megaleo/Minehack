-- | An effect is a processed version of an action, ready
-- to change tiles in the world but it only contains raw
-- information. Example:
--
-- A monster 'M' wants to attack a guy '@' with its hands,
-- then the game generates an 'Hit' action:
-- "Action 'M' '@' (Hit 'Bare_Hands')" or something like that.
-- Then an effect is returned, and it could be (ChangeInHp (-4)),
-- beacuse an normal punch attack has -2 change in hp, but in this
-- case, the monster M has an power to increase the attack when using
-- its hands.
-- In the final part, the program look at the effect and the target,
-- and generates the final state of the target tile (In this case
-- would be the loss in HP, counting with any defense tool that the
-- target could be using).

module Effect where

import qualified World as W

-- | Represent an effect to a target CTile.
data Effect = Effect W.CTile EffectType

-- | All the possible effects:
data EffectType = ChangeInPosition W.TileCoord  -- ^ Change in position.
                | ChangeInHP Int                -- ^ Change the HP by an Int.

onEffect :: W.WorldState -> Effect -> W.WorldState
onEffect (W.World _ _ tiles) (Effect (x,y) _) = undefined
--onEffect (W.World _ _ tiles) (Effect (x,y) ()) = undefined