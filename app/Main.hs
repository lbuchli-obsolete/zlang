module Main where

import Lib

main :: IO ()
main = print $ parse ":: 'id' a:_ -> a = ads"
