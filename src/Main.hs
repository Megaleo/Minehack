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

sep16 :: String -> [String]
sep16 [] = []
sep16 str = (take 16 str) : (sep16 $ drop 16 str)

vAlign :: [String] -> [String] -> [String]
vAlign = (++)

hAlign :: [String] -> [String] -> [String]
hAlign = zipWith (++)

chunkLines :: Int -> ChunkCoord -> SimpleBiome -> [String]
chunkLines seed cCoord sBiome = sep16 . chunkSymbols $ genSBiomeChunk seed cCoord sBiome

-- | Prints a Chunk.
simpleChunkPrint :: Int -> ChunkCoord -> SimpleBiome -> IO ()
simpleChunkPrint seed cCoord sBiome = simpleSymbolPrint . chunkSymbols $ genSBiomeChunk seed cCoord sBiome

chunkSymbols :: Chunk -> String
chunkSymbols = map tileSymbol . elems

-- | Main.
main :: IO ()
main = do
    putStr "Seed: "
    seed <- readLn :: IO Int
    putStr "Center Chunk Coordenates: "
    (y,x) <- readLn :: IO (Int, Int)
    putStrLn . unlines $ ((chunkLines seed (x-1,y-1) simpleForest) `hAlign` (chunkLines seed (x-1,y) simpleForest) `hAlign` (chunkLines seed (x-1,y+1) simpleForest)) `vAlign`
                         ((chunkLines seed (x,y-1) simpleForest)   `hAlign` (chunkLines seed (x,y) simpleForest)   `hAlign` (chunkLines seed (x,y+1) simpleForest))   `vAlign`
                         ((chunkLines seed (x+1,y-1) simpleForest) `hAlign` (chunkLines seed (x+1,y) simpleForest) `hAlign` (chunkLines seed (x+1,y+1) simpleForest))

