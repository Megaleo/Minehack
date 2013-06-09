{-# LANGUAGE RankNTypes, ConstraintKinds, FlexibleContexts #-}

module Main where

import UI.NCurses
import System.IO
import System.Random 
import Control.Monad
import Test.QuickCheck

import qualified World as W
import qualified Tile as T
import qualified Item as I
import qualified Block as B
import qualified Player as P
import qualified Attribute as A
import qualified Random as R

main = do
  putStrLn "Oi"
