module Item where
-- "Item.Money" for only money representention items 
--
-- Here will be all the definitions and functions around the concept of the Item.
--
-- Every Item can be stored in a slot of a container (chest, furnace, player's inventory, a box, etc.) 
--
-- An Item should be normally pickable by the player, but it couldn't be due
-- to the weight or other natural caracteristic of the item.
--
-- There will be many more other others modules and classes to specify the big "Item" class, 

import System.IO
import Control.Monad
import Data.Array

import qualified Tile as T

--Caracteristics to be an Item
class T.TileType a => Item a where  

--Item's Maximum amount of itself that can be stacked in one 
--slot on any container 
  maxStack  :: a -> Int   
 
--Item's Weight			                           
  weight    :: a -> Int    	 

