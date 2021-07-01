module Main where

import IO.Subcreator (subcreate)
import Pong.World (universe)

main :: IO ()
main = do subcreate universe
