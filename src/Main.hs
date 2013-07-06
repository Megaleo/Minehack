module Main where

-- import UI.HSCurses.Curses
-- import System.IO
-- import Control.Monad

import Data.Array

import World
import Tile
-- import Item
-- import Block
-- import Player
-- import Attribute
-- import Random

-- Prints the symbols of a Chunk.
simpleChunkPrint :: String -> IO ()
simpleChunkPrint = putStrLn . unlines . sep16
    where
        sep16 [] = []
        sep16 str = (take 16 str) : (sep16 $ drop 16 str)

-- Main.
main :: IO ()
main =
    putStr "Seed: "
	>> getLine
    >>= \x -> simpleChunkPrint . map tileSymbol . elems $ genSBiomeChunk (read x) (0,0) simpleForest
