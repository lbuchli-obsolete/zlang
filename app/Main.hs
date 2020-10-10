module Main where

import           Lib
import           System.IO

file :: String
file = "/home/lukas/workspace/haskell/zlang/zsrc/test.zl"

main :: IO ()
main = do
  contents <- readUTF8File file
  print $ typeCheck contents

readUTF8File :: String -> IO String
readUTF8File path = do
  inputHandle <- openFile path ReadMode
  hSetEncoding inputHandle utf8
  hGetContents inputHandle
