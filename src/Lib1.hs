{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use isDigit" #-}

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
  if s == "game_setup_id"
    then State (("toggles", DString []) : ((s, d) : l))
    else gameStart (State ((s, d) : l)) (DMap xs)
gameStart _ _ = emptyState

---------------------------------------------------------------------------------------------------

-- Render implementation

-- IMPLEMENT
-- Renders your game board
-- Render gets the current gamestate and turns it to a string to be printed to the screen.
-- State - The current gamestate.
-- String - The result string that has the whole gameboard and other information for displayment.

render :: State -> String
render st = "      " ++ drawGridLineNum ++ "\n ┌────────────────────────\n │    " ++ drawGridTop st ++ "\n │\n" ++ drawGrid st [] 0 ++ "\n"
  ++ drawHints st
--render = show

-- This function draws the row data and the whole grid section of the map (without the top data).
-- Meant to be used by the render function and uses drawGrid (recursively), drawGridLine functions.
-- State - The current gamestate.
-- String - A temporary string that saves the grid data. (Use [] when calling this function)
-- Int - Accumulator, which counts which row we are on. (Use 0 when calling this function)
-- String - The result string that has the row data and the grid itself.
drawGrid :: State -> String -> Int -> String
drawGrid st rez y =
  if y < 10
    then drawGrid st (rez ++ drawGridLine st y) (y + 1)
    else rez

-- This function draws a single line of the grid.
-- Meant to be used by the drawGrid function and uses drawGridSide, drawGridRow functions.
-- State - The current gamestate.
-- Int - Accumulator, which counts which row we are on.
-- String - The result string that has the line that is meant to be the output.
drawGridLine :: State -> Int -> String
drawGridLine st y = [intToChar y] ++ "│" ++ (drawGridSide st y : " ") ++ "  " ++ drawGridRow st [] 0 y ++ "\n"

-- This function draws a line from 0 to 9 as a reference point for the table.
-- Meant to be used by the render function.
-- String - The result string that is from 0 to 9 with space between them.
drawGridLineNum :: String
drawGridLineNum = foldl (\s x -> s ++ (x : " ")) [] ['0' .. '9']

-- This function draws the map side of the grid.
-- Meant to be used by the drawGridLine function and uses drawGridRow (recursively), drawGridCell functions.
-- State - The current gamestate.
-- Sring - A temporary string that saves the row data.
-- Int - Accumulator, which counts which collumn we are on.
-- Int - Accumulator, which counts which row we are on.
-- String - The result string that has the row data for a single grid row.
drawGridRow :: State -> String -> Int -> Int -> String
drawGridRow st rez x y =
  if x < 10
    then drawGridRow st (rez ++ [drawGridCell st x y] ++ " ") (x + 1) y
    else rez

-- This function draws a single cell in the grid.
-- Meant to be used by the drawGridRow function and uses getToggleValue, getHintValue functions.
-- Defines if the cell is toggled, hinted at or empty.
-- State - The current gamestate.
-- Int - Accumulator, which counts which collumn we are on.
-- Int - Accumulator, which counts which row we are on.
-- Char - The result char that shows the current state of the cell.
drawGridCell :: State -> Int -> Int -> Char
drawGridCell st x y
  | getToggleCellValue (getToggleState st) x y = 'T'
  | getHintCellValue (getHintsString (show (getHintsDoc st)) [] []) x y = 'H'
  | otherwise = '0'

-- This function finds out if a certain cell is toggled.
-- Meant to be used by the drawGridCell function and uses getToggleCellValue (recursively).
-- String - A string of currently active toggles.
-- Int - Accumulator, which counts which collumn we are on.
-- Int - Accumulator, which counts which row we are on.
-- Bool - The result that shows if the current cell is toggled.
getToggleCellValue :: String -> Int -> Int -> Bool
getToggleCellValue (xS : yS : xs) x y
  | xS == intToChar x && yS == intToChar y = True
  | null xs = False
  | otherwise = getToggleCellValue xs x y
getToggleCellValue _ _ _ = False

-- This function get the current toggle state from the gamestate.
-- Meant to be used by the drawGridCell function and uses getToggleState (recursively).
-- State - The current gamestate.
-- String - The result string that has all the active toggles.
getToggleState :: State -> String
getToggleState (State ((st, doc) : xs)) =
  if st == "toggles"
    then init (drop 9 (show doc))
    else getToggleState (State xs)
getToggleState _ = " "

getHintCellValue :: String -> Int -> Int -> Bool
getHintCellValue (xS : yS : xs) x y
  | xS == intToChar x && yS == intToChar y = True
  | null xs = False
  | otherwise = getToggleCellValue xs x y
getHintCellValue _ _ _ = False

-- This draws the side (occupied_rows) information on the board.
-- Meant to be used by the drawGridLine function and uses drawGridSide (recursively), getSingleDIntValue functions.
-- State - The current gamestate.
-- Int - Accumulator which counts which line we are on.
-- Char - The result char that will be written on the side of the board.
drawGridSide :: State -> Int -> Char
drawGridSide (State ((st, doc) : xs)) r =
  if st == "occupied_rows"
    then getSingleDIntValue (show doc) r
    else drawGridSide (State xs) r
drawGridSide _ _ = ' '

-- This gets a single char from the gamestate to put on the side (occupied_rows) information part of the board.
-- Meant to be used by the drawGridSide function and uses the getDIntValue function.
-- String - the document string, that needs to be parsed.
-- Int - Accumulator, which counts which line we are on.
-- Char - the result char that will be at the side of the grid, which represents occupied_rows
getSingleDIntValue :: String -> Int -> Char
getSingleDIntValue doc r = getDIntValue doc [] [] !! r

-- This draws the top (occupied_cols) information on the board.
-- Meant to be used by the render function and uses drawGridTop (recursively), getDIntValue functions.
-- State - The current gamestate.
-- String - The result string that will be written on the top of the board.
drawGridTop :: State -> String
drawGridTop (State ((st, doc) : xs)) =
  if st == "occupied_cols"
    then foldl (\s x -> s ++ (x : " ")) [] (getDIntValue (show doc) [] [])
    else drawGridTop (State xs)
drawGridTop _ = []

-- This gets a whole string of the gamestate information, which is shown at the top and the left side of the board.
-- Meant to be used by drawGridTop, getSingleDIntValue functions and uses the getDIntValue (recursively) function.
-- String - The document string, that needs to be parsed.
-- String - A temporary recursive string that keeps track of what we are reading.
-- String - A temporary recursive string that keeps track of what result we have collected.
-- String - The result string that is filled with chained (no whitespace characters) side information values.
getDIntValue :: String -> String -> String -> String
getDIntValue (x : xs) str rez =
  if x /= ']'
    then
      if length str == 9
        then
          if str == "DInteger "
            then getDIntValue xs [] (rez ++ [x])
            else getDIntValue xs (tail str ++ [x]) rez
        else getDIntValue xs (str ++ [x]) rez
    else rez
getDIntValue _ _ _ = "test"

---------------------------------------------------------------------------------------------------
-- Functions that currently do nothing
---------------------------------------------------------------------------------------------------
-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck (State ((str, doc) : xs)) =
  if str == "toggles"
    then convertStringToCheck (show doc)
    else mkCheck (State xs)

convertStringToCheck :: String -> Check
convertStringToCheck doc = Check (convertStringToCheck' (getToggledValues doc []))

convertStringToCheck' :: String -> [Coord]
convertStringToCheck' (x : y : xs) = Coord (convertDigitToInt x) (convertDigitToInt y) : convertStringToCheck' xs

getToggledValues :: String -> String -> String
getToggledValues (x : y : xs) rez =
  if checkDigit x
    then 
        if x /= '"' && y /= '"'
            then getToggledValues xs (rez ++ [x, y])
            else rez
    else getToggledValues (y : xs) rez        
getToggledValues _ rez = rez    

convertDigitToInt :: Char -> Int
convertDigitToInt c = fromEnum c - fromEnum '0'    
---------------------------------------------------------------------------------------------------

-- IMPLEMENT   State ((s, setToggle doc (concat str)) : xs)
-- Toggle state's value
-- Receive raw user input tokens

toggle :: State -> [String] -> State
toggle st = toggleState st (State [])

toggleState :: State -> State -> [String] -> State
toggleState (State ((s, doc) : xs)) (State temp) str =
  if s == "toggles"
    then State (temp ++ ((s, setToggle doc (concat str)) : xs))
    else toggleState (State xs) (State ((s, doc) : temp)) str
toggleState _ _ _ = emptyState

setToggle :: Document -> String -> Document
setToggle doc str = DString (readToggle (drop 9 (show doc)) (filter checkDigit str) [] 0)

readToggle :: String -> String -> String -> Int -> String
readToggle (xD : yD : xsD) (xU : yU : xsU) rez rep
  | xD /= '"' =
    if (xD == xU) && (yD == yU)
      then readToggle xsD (xU : yU : xsU) rez (rep + 1)
      else readToggle xsD (xU : yU : xsU) (rez ++ [xD] ++ [yD]) rep
  | rep == 0 = rez ++ [xU] ++ [yU]
  | otherwise = rez
readToggle ['\"'] (xU : yU : xsU) rez rep =
  if rep == 0
    then rez ++ xU : [yU]
    else rez
readToggle _ _ _ _ = ""

checkDigit :: Char -> Bool
checkDigit c =
  c >= '0' && c <= '9'

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
--hint (State l) h = State l
hint (State l) (DMap ((s, d) : xs)) = State (( "hints", d) : l)
hint _ _ = emptyState

---------------------------------------------------------------------------------------------------

drawHints :: State -> String
drawHints st = groupHints (getHintsString (show (getHintsDoc st)) [] []) []

--Finds the most current hint data and makes it to be String type
showHintState :: State -> String
showHintState (State ((st, doc) : xs)) =
  if st == "hints"
    then show doc
    else showHintState (State xs)
showHintState _ = "No hints found"

--Finds the coords in the hint STRING, uses the groupHints function to group coords and gives us what user sees
getHintsString :: String -> String -> String -> String
getHintsString (x : xs) str rez
  | x /= 'N' =
    if length str == 9
      then
        if str == "DInteger "
          then getHintsString xs [] (rez ++ [x])
          else getHintsString xs (tail str ++ [x]) rez
      else getHintsString xs (str ++ [x]) rez
  | length rez == 0 = "No hints found"
  | otherwise = rez
getHintsString _ _ _ = "No hints found"

-- From one line of numbers (coordinates) makes it look like "(6;8)"
groupHints :: String -> String -> String
groupHints (x : y : xs) rez
  | x : [y] == "No" = x : y : xs
  | otherwise = groupHints xs (rez ++ " (" ++ [x] ++ ";" ++ [y] ++ ") ")
groupHints _ rez = rez

--Right now it is not used!!!!!!!!!!!!!!!!!
getHintsDoc :: State -> Document
getHintsDoc (State ((x, s) : xs)) =
  if x == "hints"
    then s
    else getHintsDoc (State xs)
getHintsDoc _ = DNull

hintExists :: State -> Bool
hintExists (State ((str, doc) : xs)) = str == "hints"
hintExists _ = False

--Dont forget to delete this if Andrius likes hints as they are right now

{-
getHintList :: Document -> Document -> [Document]

getHintList (DMap ((s, d) : xs)) DList(a : as) =
  if s == "col" || s == "row"
    then getHintList(DMap d) DList(as ++ d)
  else getHintList (DMap d) DList(as ++ d)
getHintList _ _= []

-}

{-
getHintDoc :: String -> Document -> Document -> Document

getHintDoc str (DMap ((s, d) : xs)) (DString a) =
  a

getHintDoc _ _ _ = DString("No hints found")
-}

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

-- Useful helper functions.

intToChar :: Int -> Char
intToChar x
  | x == 0 = '0'
  | x == 1 = '1'
  | x == 2 = '2'
  | x == 3 = '3'
  | x == 4 = '4'
  | x == 5 = '5'
  | x == 6 = '6'
  | x == 7 = '7'
  | x == 8 = '8'
  | x == 9 = '9'
intToChar _ = ' '

charToInt :: Char -> Int
charToInt x
  | x == '0' = 0
  | x == '1' = 1
  | x == '2' = 2
  | x == '3' = 3
  | x == '4' = 4
  | x == '5' = 5
  | x == '6' = 6
  | x == '7' = 7
  | x == '8' = 8
  | x == '9' = 9
charToInt _ = 0
