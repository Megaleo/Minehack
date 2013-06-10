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

class (Show a, Eq a) => Attribute a where

--Attribute's unique ID
  id      :: a -> Int
  
--Attribute's unique name  
  name    :: a -> String
  
--Attribute's Effects
--effects :: [Effect]

-- Datatype for Attribute class
data DataAttribute = Attribute { aId   :: Int
                               , aName :: String } deriving (Show, Eq)

-- Converts an Attribute to a DataAttribute 
attributeData :: Attribute a => a -> DataAttribute
attributeData attribute = Attribute { aId   = Attribute.id attribute
                                    , aName = name attribute}

-- "Converts" a DataAttribute to a Attribute
instance Attribute DataAttribute where
  id   (Attribute aId _)   = aId
  name (Attribute _ aName) = aName

-- Attributes for testing purposes

data NoAttributes = NoAttributes deriving (Show, Eq)
instance Attribute NoAttributes where
  id   _ = -1
  name _ = "NoAttributes" 
