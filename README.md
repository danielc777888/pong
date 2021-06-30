# pong
The venerable pong

## Dependencies
* The raylib c library
* Currently haskell base/standard libs : https://downloads.haskell.org/ghc/latest/docs/html/users_guide/8.10.1-notes.html#ghc-library
* Main -> (IO, Pong) , Pong -> Core, IO -> Core

## Building/Running
* Build/install raylib as SHARED library. https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux . Commit hash : 88a6f16c9a552ebb8c39fff57cf16cfce7c88913
* To build execute : cabal build --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
* To run execute : cabal run --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
* Creating spritesheets : aseprite -b lpaddle.png rpaddle.png --sheet-type rows --sheet paddles.png
* Tested with cabal 3.2.0.0, ghc 8.10.1

## Screenshot
![Screenshot image](https://github.com/danielc777888/pong/blob/main/screenshot.png "Screenshot")