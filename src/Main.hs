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

-- | Prints the symbols of a Chunk.
simpleSymbolPrint :: String -> IO ()
simpleSymbolPrint = putStrLn . unlines . sep16
    where
        sep16 [] = []
        sep16 str = (take 16 str) : (sep16 $ drop 16 str)

-- | Prints a Chunk.
simpleChunkPrint :: Int -> ChunkCoord -> SimpleBiome -> IO ()
simpleChunkPrint seed cCoord sBiome = simpleSymbolPrint . map tileSymbol . elems $ genSBiomeChunk seed cCoord sBiome

-- | Main.
main :: IO ()
main = do
    putStr "Seed: "
    seed <- readLn :: IO Int
    putStr "Chunk Coordenates: "
    cCoord <- readLn :: IO (Int, Int)
    simpleChunkPrint seed cCoord simpleForest
