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

-- import qualified Random as R

data Item = Wood 
          deriving Eq

-- | Item's maximum amount of itself that can be stacked in one
-- | slot on any container.
maxStack      :: Item -> Int
maxStack Wood = 64 

-- | Item's weight in grams per m^3
weight    :: Item -> Double
weight Wood = 700