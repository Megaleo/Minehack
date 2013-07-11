module Input where

import System.IO

setEcho :: Bool -> IO ()
setEcho = hSetEcho stdin

getHiddenChar :: IO Char
getHiddenChar = do
    setEcho False
    getChar >>= return
