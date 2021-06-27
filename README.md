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

## Conventions for modules

*Export public functions/types
*Import module functions/types
*If conflict , prefix with module alias

## Conventions for types/data

*Initially use type synonym
*If need to use type class, or union type or recursive. Use data decleration
*If constructor with one argument use newtype
*It much conflicts with functions/selectors consider creating type class for overloaded function

## TODO

* Make functions/types explicit in imports
* Use latest pinned raylib version.

