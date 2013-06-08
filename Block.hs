module Block where
-- "Block.RawMaterial" for block's raw materials (There are blocks!) 
--
-- Here will be all the definitions and functions around the concept of the Block.

import System.IO
import Control.Monad
import Data.Array

import qualified Tile as T
import qualified Random as R

-- Caracteristics to be an Block (class or data?)
class T.TileType a => Block a where

--Block's Visibility 
  seeThrough  :: a -> Bool --seeThrough :: a -> ColorSpectrum -> Bool       

--Block's "Walk and stand over" caracteristic (with damage value)                    
  walkThrough :: a -> R.Roll -> Bool  

--Block's Fixed amount of hits to break (Initial damage value)		    	         
  hitstoBreak :: a -> R.Roll 

--Block's Weight		    	         
  weight      :: a -> Int -- weight :: a -> a -> Weight         
				           
--Block's drop when broken				           
  drops       :: (T.TileType b) => a -> Maybe [b]          
                             
--Block's physical material
--material    :: a -> [Block.RawMaterial]
                            
