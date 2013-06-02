module Main where

import UI.NCurses
import System.IO
import System.Random 
import Control.Monad

import qualified World as W
import qualified Tile as T
import qualified Item as I
import qualified Block as B
import qualified Player as P
import qualified Attribute as A

main = do
  putStrLn "Oi"
