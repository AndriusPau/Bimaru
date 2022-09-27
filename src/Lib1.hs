{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib1
  ( State,
    emptyState,
    gameStart,
    render,
    mkCheck,
    toggle,
    hint,
  )
where

import Types

{-
---------------------------------------------------------------------------------------------------
-- Ignore this code fragment
---------------------------------------------------------------------------------------------------
-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State {
    numberOfHints :: Int,
    occupiedCols :: [Int],
    occupiedRows :: [Int],
    hints :: [Coord],
    toggles :: [Coord],
    position :: [(Coord, Char)]
    }
    deriving Show

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State []

-- IMPLEMENT
-- This adds game data to initial state
gameStart :: State -> Document -> State
gameStart (State l) d = State $ (numberOfHints ++ occupiedCols ++ occupiedRows) : l
--"Game started: " ++ "|||" ++ numberOfHints ++ "|||" ++ occupiedCols ++ "|||" ++ occupiedRows ++ "|||" ++ gameId
    where
        doc = show d
        numberOfHints = doc !! 34 : doc !! 35 : " "
        occupiedCols = doc !! 71 : doc !! 82 : doc !! 93 : doc !! 104 : doc !! 115 : doc !! 126 : doc !! 137 : doc !! 148 : doc !! 159 : doc !! 170 : " "
        occupiedRows = doc !! 207 : doc !! 218 : doc !! 229 : doc !! 240 : doc !! 251 : doc !! 262 : doc !! 273 : doc !! 284 : doc !! 295 : doc !! 306 : " "
        -- gameId = take 36 $ drop 336 doc

--gameStart :: State -> Document -> State
--gameStart s d =
---------------------------------------------------------------------------------------------------
-}

---------------------------------------------------------------------------------------------------
-- State settings
data State = State [(String, Document)]
  deriving (Show)

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State []

-- IMPLEMENT
-- This adds game data to initial state
gameStart :: State -> Document -> State
gameStart (State l) (DMap ((s, d) : xs)) =
  if s /= "game_setup_id"
    then gameStart (State $ (s, d) : l) (DMap xs)
    else State $ (s, d) : l
gameStart _ _ = emptyState
---------------------------------------------------------------------------------------------------

-- Render implementation

-- IMPLEMENT
-- renders your game board
-- Render gets the current gamestate and turns it to a string to be outputted.
-- Implementing a Render function which will hopefully make the gamestate string actually readable.
render :: State -> String
render = show
{-
render (State ((s, d) : xs)) =
  case s of
    "game_setup_id" -> show d ++ "\n" : render (State xs)
    "occupied_rows" -> show d ++ "\n"
    "occupied_cols" -> show d ++ "\n"
    "number_of_hints" -> show d ++ "\n" 
-}

-- IMPLEMENT drawGridLine, drawGridSide, drawGridBottom,
-- After these functions are applied, show cannot be used any further.

-- [(Int, Int)] - toggle coordinates.
-- [(Int, Int)] - hint coordinates.
-- Int - Accumulator, which counts which line we are on. 
-- [Char] - The row of chars that will be outputted.
drawGridLine :: [(Int, Int)] -> [(Int, Int)] -> Int -> [Char]
drawGridLine [(x1, y1)] [(x2, y2)] z = show x1
drawGridLine [(_, _)] [(_, _)] _ = []


-- [Int] - occupied_rows list
-- Int - Accumulator which counts which line we are on.
-- Char - The number that will be written on the side of the board.
drawGridSide :: [Int] -> Int -> Char
drawGridSide [] _ = ' '


-- [String] - occupied_cols list
-- [Char] - Temporary function list
-- [Char] - The row of chars that will be at the bottom of the grid, which represents occupied_cols
drawGridBottom :: String -> [Char] -> [Char]
drawGridBottom (x : xs) l = drawGridBottom xs l


-- where
-- [occupiedCols]
-- [occupiedRows]
-- [[currentMap]]

-- show l


---------------------------------------------------------------------------------------------------
-- Functions that currently do nothing
---------------------------------------------------------------------------------------------------
-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State l) t = State l

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State l) h = State l
---------------------------------------------------------------------------------------------------

{-
---------------------------------------------------------------------------------------------------
-- Unchanged (starter) functions

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck _ = Check []

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State l) t = State $ ("Toggle " ++ show t) : l

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State l) h = State $ ("Hint " ++ show h) : l
---------------------------------------------------------------------------------------------------
-}