# pong
Unfinished pong game in basic Haskell

## Dependencies
* The raylib c library
* Currently haskell base/standard libs : https://downloads.haskell.org/ghc/latest/docs/html/users_guide/8.10.1-notes.html#ghc-library
* Main -> (IO, Pong) , Pong -> Core, IO -> Core
* Pong : Game specific purely functional code
* Core : Game agnostic purely functional code
* IO : Game agnostic IO code

## Building/Running
* Build/install raylib as SHARED library. https://github.com/raysan5/raylib/wiki/Working-on-GNU-Linux . Commit hash : 88a6f16c9a552ebb8c39fff57cf16cfce7c88913
* To build execute : cabal build --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
* To run execute : cabal run --extra-include-dirs=/usr/local/include --extra-lib-dirs=/usr/local/lib
* Creating spritesheets : aseprite -b lpaddle.png rpaddle.png --sheet-type rows --sheet paddles.png
* Tested with cabal 3.2.0.0, ghc 8.10.1

## Design
* First player to 5 wins
* As time goes by game speeds up, every time ball hit paddle speed.
## Screenshot
![Screenshot image](https://github.com/danielc777888/pong/blob/main/screenshot.png "Screenshot")

## TODO
* Use raylib pinned version. Currently 3.7.0
* Migrate to latest ghc 9.2.*, use RecordDotSyntax extension
* Animation of paddles, use world to to animate

## Conventions
* Use record syntax for data types
* Export/import module be explicit with function/types, to mimimize ambiguity
* Dont have to use record syntax for everything.
        -> Try first to use  pattern matching or succint data construction. If that gets unwieldy then use record syntaxt
* Use module alias to resolve ambiguity which cannot be avoided
