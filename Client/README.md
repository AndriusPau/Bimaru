# bimaru-client

We will solve [bimaru](https://en.wikipedia.org/wiki/Battleship_(puzzle)) puzzles!

Steps:
1. Clone this repository (in the future you will have to merge this repo into your repo multiple times)
2. Install [GHCup](https://www.haskell.org/ghcup/)
3. Run `ghcup tui` and install:
    - GHC 8.10.7
    - Recommended version of Stack
    - Recommended version of HLS
4. (Optional) Install VSCode with Haskell plugin
5. Run `stack build` to see if everything works
6. Run `stack run -- $YOUR_TOKEN` play a game

Some other notes:
- On Windows you might need to install "Msys2"
- On Linux you might need to install "libtinfo-dev" and "zlib1g-dev"
- On Linux you might need to use `stack` with version suffix, e.g. `stack-2.7.5`. 
