{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
module Lib4(emptyState, State (..), gameStart, GameStart, render, renderDocument
    ) where

import Types ( Document(..), GameStart(..))
import Control.Applicative
import Data.Char

-- State settings
newtype State = State [(String, Document)]
  deriving (Show)

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State []

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State l) (GameStart doc) = gameStartRecursive (State l) doc
-- gameStart (State l) (GameStart doc) = error $ show l ++ "\n\n\n" ++ show doc

gameStartRecursive :: State -> Document -> State
gameStartRecursive (State l) (DMap ((s, d) : xs)) =
  case s of
    "game_setup_id" -> gameStartRecursive (State ((s, d) : l)) (DMap xs)
    "occupied_rows" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "occupied_cols" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "number_of_hints" -> gameStartRecursive (State ((s, d) : l)) (DMap xs)
    _ -> gameStartRecursive (State (("toggles", DString []) : l)) (DMap xs)
gameStartRecursive (State l) _ = State (("toggles", DString []) : l)


-- This gets a whole string of the gamestate information, which is shown at the top and the left side of the board.
-- Meant to be used by drawGridTop, getSingleDIntValue functions and uses the getDIntValue (recursively) function.
-- String - The document string, that needs to be parsed.
-- String - A temporary recursive string that keeps track of what we are reading. (Use [] when calling this function)
-- String - A temporary recursive string that keeps track of what result we have collected. (Use [] when calling this function)
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
-- Render implementation

-- IMPLEMENT
-- Renders your game board
-- Render gets the current gamestate and turns it to a string to be printed to the screen.
-- Uses drawGridLineNum, drawGridTop, drawGrid functions.
-- State - The current gamestate.
-- String - The result string that has the whole gameboard and other information for displayment.
render :: State -> String
render st = "      " ++ drawGridLineNum ++ "\n ┌────────────────────────\n │    " ++ drawGridTop st ++ "\n │\n" ++ drawGrid st [] 0

-- render = show

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
-- Sring - A temporary string that saves the row data. (Use [] when calling this function)
-- Int - Accumulator, which counts which collumn we are on. (Use 0 when calling this function)
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
  | getHintCellValue (getHintState st) x y = 'H'
  | otherwise = '.'

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

-- This function finds out if a certain cell is hinted at.
-- Meant to be used by the drawGridCell function and uses getHintCellValue (recursively).
-- String - A string of currently active hints.
-- Int - Accumulator, which counts which collumn we are on.
-- Int - Accumulator, which counts which row we are on.
-- Bool - The result that shows if the current cell is hinted at.
getHintCellValue :: String -> Int -> Int -> Bool
getHintCellValue (xS : yS : xs) x y
  | xS == intToChar x && yS == intToChar y = True
  | null xs = False
  | otherwise = getHintCellValue xs x y
getHintCellValue _ _ _ = False

-- This function get the current hint state from the gamestate.
-- Meant to be used by the drawGridCell function and uses getHintState (recursively).
-- State - The current gamestate.
-- String - The result string that has all the active hints.
getHintState :: State -> String
getHintState (State ((st, doc) : xs)) =
  if st == "hints"
    then init (drop 9 (show doc))
    else getHintState (State xs)
getHintState _ = " "

-- This draws the side (occupied_rows) information on the board.
-- Meant to be used by the drawGridLine function and uses drawGridSide (recursively), getSingleDIntValue functions.
-- State - The current gamestate.
-- Int - Accumulator which counts which line we are on.
-- Char - The result char that will be written on the side of the board.
drawGridSide :: State -> Int -> Char
drawGridSide (State ((st, doc) : xs)) r =
  if st == "occupied_rows"
    then show doc !! (r + 9)
    else drawGridSide (State xs) r
drawGridSide _ _ = ' '

-- This draws the top (occupied_cols) information on the board.
-- Meant to be used by the render function and uses drawGridTop (recursively), getDIntValue functions.
-- State - The current gamestate.
-- String - The result string that will be written on the top of the board.
drawGridTop :: State -> String
drawGridTop (State ((st, doc) : xs)) =
  if st == "occupied_cols"
    then foldl (\s x -> s ++ (x : " ")) [] (init (drop 9 (show doc)))
    else drawGridTop (State xs)
drawGridTop _ = []

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument = parseDocToYaml


--
-- for parsing a document to YAML with indentation
--

parseDocToYaml :: Document -> String

parseDocToYaml doc = docToString doc "" 0


docToString :: Document -> String -> Int -> String

docToString DNull acc _ = acc ++ "null" ++ "\n"

docToString (DInteger x) acc _ = acc ++ show x ++ "\n"

docToString (DString x) acc _ = acc ++ show x ++ "\n"

docToString (DList(DMap x: xs)) acc nest =
  let result = docToString (DMap x) (acc ++ printSpaces nest acc ++ "- ") (nest + 1) in
  result ++ docToString' (DList xs) "" nest

docToString (DList(DList x: xs)) acc nest =
  let result = docToString (DList x) (acc ++ printSpaces nest acc ++ "- ") (nest + 1) in
  result ++ docToString' (DList xs) "" nest

docToString(DList (x : xs)) acc nest =
  let result = docToString x (acc ++ printSpaces nest acc++ "- ") nest in
  result ++ docToString' (DList xs) "" nest

docToString(DList[]) acc _ = acc ++ "[]\n"

docToString (DMap ((str, DMap []) : xs)) acc nest =
  let result = docToString (DMap []) (acc ++ printSpaces nest acc ++ str ++ ": ") (nest + 1) in
  result ++ docToString' (DMap xs) "" nest

docToString (DMap ((str, DMap doc) : xs)) acc nest =
  let result = docToString (DMap doc) (acc ++ printSpaces nest acc ++ str ++ ":\n") (nest + 1) in
  result ++ docToString' (DMap xs) "" nest

docToString (DMap ((str, DList []) : xs)) acc nest =
  let result = docToString (DList []) (acc ++ printSpaces nest acc ++ str ++ ": ") nest in
  result ++ docToString' (DMap xs) "" nest

docToString (DMap ((str, DList doc) : xs)) acc nest =
  let result = docToString (DList doc) (acc ++ printSpaces nest acc ++ str ++ ":\n") nest in
  result ++ docToString' (DMap xs) "" nest

docToString (DMap ((str, doc) : xs)) acc nest =
  let result = docToString doc (acc ++ printSpaces nest acc ++ str ++ ": ") nest in
  result ++ docToString' (DMap xs) "" nest

docToString (DMap[]) acc _ = acc ++ "{}\n"


docToString' :: Document -> String -> Int -> String

docToString' DNull acc _ = acc ++ "null" ++ "\n"

docToString' (DInteger x) acc _ = acc ++ show x ++ "\n"

docToString' (DString x) acc _ = acc ++ show x ++ "\n"

docToString' (DList (DMap x : xs)) acc nest =
  let result = docToString (DMap x) (acc ++ printSpaces nest acc ++ "- ") (nest + 1) in
  result ++ docToString' (DList xs) "" nest

docToString' (DList (DList x : xs)) acc nest =
  let result = docToString (DList x) (acc ++ printSpaces nest acc ++ "- ") (nest + 1) in
  result ++ docToString' (DList xs) "" nest


docToString' (DList (x : xs)) acc nest=
  let result = docToString x (acc ++ printSpaces nest acc ++ "- ") nest in
  result ++ docToString' (DList xs) "" nest

docToString' (DList []) acc _ = acc

docToString' (DMap ((str, DMap []) : xs)) acc nest =
  let result = docToString (DMap []) (acc ++ printSpaces nest acc ++ str ++ ": ") (nest + 1) in
  result ++ docToString' (DMap xs) "" nest

docToString' (DMap ((str, DMap doc) : xs)) acc nest =
  let result = docToString (DMap doc) (acc ++ printSpaces nest acc ++ str ++ ":\n") (nest + 1) in
  result ++ docToString' (DMap xs) "" nest

docToString' (DMap ((str, DList doc) : xs)) acc nest =
  let result = docToString (DList doc) (acc ++ printSpaces nest acc ++ str ++ ": ") nest in
  result ++ docToString' (DMap xs) "" nest

docToString' (DMap ((str, doc) : xs)) acc nest =
  let result = docToString doc (acc ++ printSpaces nest acc ++ str ++ ": ") nest in
  result ++ docToString' (DMap xs) "" nest

docToString' (DMap[]) acc _ = acc



printSpaces :: Int -> String -> String

printSpaces 0 _ = ""

printSpaces count [] =
  "  " ++ printSpaces (count - 1) []

printSpaces count str =
  if take 2 (reverse str) /= " -"
    then"  " ++ printSpaces (count - 1) str
  else ""

---------------------------------------------------------------------------------------------------
-- Useful helper functions.

-- Converts a digit in int format to a char
-- Int - The digit in int format.
-- Char - The result char.
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

-- Converts a digit in char format to an int
-- Char - The digit in char format.
-- Int - The result int.
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

-- Checks if the char is a number.
-- Char - The char...
-- Bool - The result...
checkDigit :: Char -> Bool
checkDigit c =
  c >= '0' && c <= '9'