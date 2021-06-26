# pong
The venerable pong

## Dependencies
* The raylib c library
* Currently haskells base/standard libs : https://downloads.haskell.org/ghc/latest/docs/html/users_guide/8.10.1-notes.html#ghc-library
* Main -> (IO, Pong) , Pong -> Core, IO -> Core

## Building/Running

* Build/install raylib as SHARED library. https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux
* To build execute : cabal build --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
* To run execute : cabal run --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
* Creating spritesheets : aseprite -b lpaddle.png rpaddle.png --sheet-type rows --sheet paddles.png

## Conventions for modules/conflicts

*Export public functions
*Ensure record field names are prefixed, for game specific modules. ie. Pong
*First just import module
*If conflict make explicit function import OR can name be changed??
*If still conflict use qualified import. For standard lib use 1 char(eg. M), otherwise module name(eg. Paddle)
