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
* As time goes by game speeds up, every time ball hit increase paddle speed.

## Self-imposed Constraints
* Dependencies only core/std libs and raylib
* Lazy all the way. No explicit use of strictness features
* No explicit use of language extensions. Haskell2010 compliant. https://www.haskell.org/definition/haskell2010.pdf
* Monadic code only when interacting with IO

## Screenshot
![Screenshot image](https://github.com/danielc777888/pong/blob/main/screenshot.png "Screenshot")

## TODO
* Use raylib pinned version. Currently 3.7.0
* Migrate to latest reccommended ghc with ghcup
* Remove use of all non core lib import aliases
* Import explicit functions, data
* Prefix Raylib Keys with Ray to prevent conflicts
* Randomely generate placement of new ball
* Physics ball, paddles
* Detect goal, and increment scores
* Display current score
* Save num wins for Player(A), Player(B)
* (P)ause game, (Q)uit game with confirmation
* Create start screen with : (S)tart, (Q)uit options, display current score tally
* Create game with 4 pong games on one screen

## Conventions
* Use record syntax for data types
* When conflict with function name, prefix with globally unique 3 lower case chars
* Export/import module be explicit with function/types, to mimimize ambiguity
* Use import qualified for std libs/core conflicts