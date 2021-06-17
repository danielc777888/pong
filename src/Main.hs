module Main where

import IO.Subcreator
import Pong.World
import Pong.Universe

main :: IO ()
main = do subcreate world universe
