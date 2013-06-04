module World where
--
-- The way to handle "infinite" world elements is, in the nutshell, load 16x16 Tiles chunks with 
-- random generation (seed) and verify if any of those have modified tiles.
--
-- This module will care of these world stuff, loading chunks mainly.


import System.IO
import Control.Monad
import Data.Array



