module Main where

import IO.Subcreator (subcreate)
import Pong.Universe (universe)

main :: IO ()
main = do subcreate universe
