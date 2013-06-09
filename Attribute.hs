module Attribute where
-- All the attributes given to a Tile (TileType) by something or someone in-game
--
-- #@ Examples @#
-- 
-- -# Fire Resistance #-
-- @ Gives you resistance to fire 
-- @ May be given by a potion
--
-- -# Illness #-
-- @ Slowly removes your HP
-- @ May be given randomly in some circumstances
--
-- -# Fatique #-
-- @ Slows down your moviments and attacks
-- @ May be given by a potion or running without resting/eating 


import System.IO
import Control.Monad
import Data.Array

class Attribute a where

--Attribute's unique ID
  id      :: a -> Int
  
--Attribute's unique name  
  name    :: a -> String
  
--Attribute's Effects
--effects :: [Effect]

data NoAttributes = NoAttributes
instance Attribute NoAttributes where
  id   _ = -1
  name _ = "No Attributes" 
