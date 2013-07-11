module Input where

import System.IO

getHiddenChar :: IO Char
getHiddenChar = do
    hSetEcho stdin False
    getChar >>= return