module Main where

import Lib
import System.IO

file :: String
file = "/home/lukas/workspace/haskell/zlang/zsrc/test.zl"

-- main :: IO ()
-- main = do
--   handle <- openFile file ReadMode
--   contents <- hGetContents handle
--   print $ typeCheck contents

main :: IO ()
main = do
  print $ typeCheck ":: 'add' I64 | F64 -> I64 = 35"

