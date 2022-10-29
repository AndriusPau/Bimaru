module Lib1
  ( State(..),
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
newtype State = State [(String, Document)]
  deriving (Show)

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State []

-- IMPLEMENT
-- This adds game data to initial state
gameStart :: State -> Document -> State
gameStart (State l) (DMap ((s, d) : xs)) =
  case s of
    "game_setup_id" -> State (("toggles", DString []) : ("hints", DString []) : ((s, d) : l))
    "occupied_rows" -> gameStart (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "occupied_cols" -> gameStart (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    _ -> gameStart (State ((s, d) : l)) (DMap xs)
gameStart _ _ = emptyState

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
-- Check implementation

-- IMPLEMENT
-- Returns the current toggled cells which is sent to the server and checked if it is correct.
-- Uses convertStringToCheck, mkCheck (recursively) functions.
-- State - Current game state.
-- Check - The result Check type that contains a list of toggled cells and is sent to the server.
mkCheck :: State -> Check
mkCheck (State ((str, doc) : xs)) =
  if str == "toggles"
    then convertStringToCheck (show doc)
    else mkCheck (State xs)
mkCheck _ = Check []

-- This function converts the document string into a Check data type.
-- Meant to be used by the mkCheck function and uses convertStringToCheck', getToggledValues functions.
-- String - The document string, that needs to be parsed.
-- Check - The result Check data type that contains a list of toggled cells.
convertStringToCheck :: String -> Check
convertStringToCheck doc = Check (convertStringToCheck' (getToggledValues doc []))

-- This function converts the string of toggled cells to a list of coordinates.
-- Meant to be used by the convertStringToCheck function and uses convertStringToCheck' (recursively), charToInt functions.
-- String - The document string, that needs to be parsed.
-- [Coord] - A list of coordinates that contains toggled cells in a (col,row) format.
convertStringToCheck' :: String -> [Coord]
convertStringToCheck' (x : y : xs) = Coord (charToInt x) (charToInt y) : convertStringToCheck' xs
convertStringToCheck' _ = []

-- This function gets a string of toggled cells from the gamestate.
-- Meant to be used by the convertStringToCheck function and uses getTogglesValues (recursively), checkDigit functions.
-- String - The document string, that needs to be parsed.
-- String - A temporary recursive string that keeps track of the result. (Use [] when calling this function)
-- String - The result string that contains all the toggled cells without whitespaces.
getToggledValues :: String -> String -> String
getToggledValues (x : y : xs) rez =
  if checkDigit x
    then
      if x /= '"' && y /= '"'
        then getToggledValues xs (rez ++ [x, y])
        else rez
    else getToggledValues (y : xs) rez
getToggledValues _ rez = rez

---------------------------------------------------------------------------------------------------
-- Toggle implementation

-- IMPLEMENT
-- Toggle state's value. Receive raw user input tokens.
-- Uses the toggleState function.
-- State - The current gamestate.
-- [String] - The user input list of strings.
-- State - The result state after the use of toggle.
toggle :: State -> [String] -> State
toggle st = toggleState st (State [])

-- This function changes the gamestate after the user uses the toggle function.
-- Meant to be used by the toggle function and uses toggleState (recursively), setToggle functions.
-- State - The current gamestate.
-- State - A temporary recursive state that keeps track of state changes. (Use (State []) when calling this function)
-- [String] - A list of strings that contains the toggle information.
-- State - The result state after the use of a toggle.
toggleState :: State -> State -> [String] -> State
toggleState (State ((s, doc) : xs)) (State temp) str =
  if s == "toggles"
    then State (temp ++ ((s, setToggle doc (concat str)) : xs))
    else toggleState (State xs) (State ((s, doc) : temp)) str
toggleState _ _ _ = emptyState

-- This function changes the gamestate document in accordance to the user input.
-- Meant to be used by the toggleState function and uses readToggle, checkDigit functions.
-- Document - The current toggle state in the gamestate.
-- String - The user input after using he toggle function.
-- Document - The result toggle state after the changes.
setToggle :: Document -> String -> Document
setToggle doc str = DString (readToggle (init (drop 9 (show doc))) (filter checkDigit str) [] 0)

-- This function reads the document string and changes it to fit the toggle condition.
-- Meant to be used by the setToggle function and uses the readToggle (recursively) function.
-- String - The current gamestate document string.
-- String - The user input string.
-- String - A temporary recursive string that keeps track of state changes. (Use [] when calling this function)
-- Int - Accumulator, which counts if there were any changes to the document string. (Use 0 when calling this function)
-- String - The result string that contains the updated gamestate toggle information.
readToggle :: String -> String -> String -> Int -> String
readToggle (xD : yD : xsD) (xU : yU : xsU) rez rep
  | xD == xU && yD == yU = readToggle xsD (xU : yU : xsU) rez (rep + 1)
  | xD /= xU || yD /= yU = readToggle xsD (xU : yU : xsU) (rez ++ [xD] ++ [yD]) rep
  | (rep == 0) && not (null xsU) = readToggle (rez ++ [xU] ++ [yU]) xsU [] 0
  | (rep == 0) && null xsU = rez ++ [xU] ++ [yU]
  | (rep == 1) && not (null xsU) = readToggle rez xsU [] 0
  | (rep == 1) && null xsU = rez
  | otherwise = rez
readToggle [] (xU : yU : xsU) rez rep
  | rep == 0 =
    if null xsU
      then rez ++ [xU] ++ [yU]
      else readToggle (rez ++ [xU] ++ [yU]) xsU [] 0
  | null xsU = rez
  | otherwise = readToggle rez xsU [] 0
readToggle xsD [_] _ _ = xsD
readToggle xsD [] _ _ = xsD
readToggle rez _ _ _ = rez ++ "test"

---------------------------------------------------------------------------------------------------
-- Hint implementation

-- IMPLEMENT
-- Adds hint data to the game state
-- Uses the hintState function.
-- State - The current gamestate.
-- Document - The hint information gained from the server.
-- State - The result gamestate after the use of a hint.
hint :: State -> Document -> State
hint (State l) (DMap ((_, d) : _)) = hintState (State l) (State []) d
hint _ _ = emptyState

-- This function located the hint data in the current gamestate.
-- Meant to be used by the hint function and uses hintState (recursively), setHint functions.
-- State - The current gamestate.
-- State - A temporary recursive state that keeps track of state changes. (Use (State []) when calling this function)
-- Document - The document that is applied to the gamestate, before parsing.
-- State - The result gamestate after applying the hint changes.
hintState :: State -> State -> Document -> State
hintState (State ((st, doc) : xs)) (State temp) d =
  if st == "hints"
    then State (temp ++ (("hints", setHint d) : xs))
    else hintState (State xs) (State (temp ++ [(st, doc)])) d
hintState _ _ _ = emptyState

-- This function changes the document in a way to suit the state.
-- Meant to be used by the hintState function and uses the getHintString function.
-- Document - The document that is applied to the gamestate, before parsing.
-- Document - The result document that contains the changed hint information.
setHint :: Document -> Document
setHint doc = DString (getHintsString (show doc) [] [])

-- This function finds the coordinate information from the doument string.
-- Meant to be used by the setHint function and uses the getHintString (recursively) function.
-- String - The document string, before parsing.
-- String - A temporary recursive string that keeps track of the position in the document. (Use [] when calling this function)
-- String - A temporary recursive string that keeps track of the parsed results. (Use [] when calling this function)
-- String - The result string that contains the state changes made.
getHintsString :: String -> String -> String -> String
getHintsString (x : xs) str rez
  | x /= 'N' =
    if length str == 9
      then
        if str == "DInteger "
          then getHintsString xs [] (rez ++ [x])
          else getHintsString xs (tail str ++ [x]) rez
      else getHintsString xs (str ++ [x]) rez
  | null rez = ""
  | otherwise = rez
getHintsString _ _ _ = ""

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