module Lib2(renderDocument, hint, gameStart) where

import Types ( Document(..))
import Lib1 (State(..))

-- !
-- How to enter GHCI:
-- Open the terminal.
-- Enter the directory where your files are located (src folder most likely) on your terminal.
-- Write "stack exec --package async --package say -- ghci".
-- You should see that you're in the prelude section of the GHCI.
-- If so, write ":load FILE_NAME" (FILE_NAME is most likely "lib2").
-- This should have loaded all the necessary files.
-- Use this environment to test out your functions.
-- Example:
-- "renderDocument (DString "test")"
-- This line will call the renderDocument function with the input (DString "test") and give you an output.
-- GL

-- Correct Toggle coordinates:
-- 0205233345273747626365666768808188949596
-- 02 05 23 33 45 27 37 47 62 63 65 66 67 68 80 81 88 94 95 96

-- Working yaml formats, that the server can accept:
-- "{coords: [{col: 0, row: 2}, {col: 0, row: 5}, {col: 2, row: 3}, {col: 3, row: 3}, {col: 4, row: 5}, {col: 2, row: 7}, {col: 3, row: 7}, {col: 4, row: 7}, {col: 6, row: 2}, {col: 6, row: 3}, {col: 6, row: 5}, {col: 6, row: 6}, {col: 6, row: 7}, {col: 6, row: 8}, {col: 8, row: 0}, {col: 8, row: 1}, {col: 8, row: 8}, {col: 9, row: 4}, {col: 9, row: 5}, {col: 9, row: 6}]}"

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument doc = parseDoc doc []

-- This is very initial state of your program
emptyState :: State
emptyState = State []

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State l) d
  | not (existsHintInfo d "coords") = Left "No coordinate info found."
  | not (checkIfCorrectCoordInfo d) = Left "Incorrect coordinate info."
  | show (hints (State l) d) == show emptyState = Left "Hints not found in passed state."
  | otherwise = Right $ hints (State l) d

hints :: State -> Document -> State
hints (State l) (DMap ((_, d) : _)) = hintState (State l) (State []) d
hints _ _ = emptyState

hintState :: State -> State -> Document -> State
hintState (State ((st, doc) : xs)) (State temp) d =
  if st == "hints"
    then State (temp ++ (("hints", setHint d) : xs))
    else hintState (State xs) (State (temp ++ [(st, doc)])) d
hintState _ _ _ = emptyState

setHint :: Document -> Document
setHint doc = DString (getHintsString (show doc) [] [])

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

existsHintInfo :: Document -> String -> Bool
existsHintInfo (DMap ((s, d) : _)) str
  | s == str = True
  | otherwise = existsHintInfo d str
existsHintInfo _ _ = False

--Recursively checks if each col has a coresponding row
checkIfCorrectCoordInfo :: Document -> Bool
checkIfCorrectCoordInfo (DMap ((s, d) : _))
  | s == "coords" && d == DNull = True
  | s == "coords" = checkIfCorrectCoordInfo' d
  | otherwise = checkIfCorrectCoordInfo d
checkIfCorrectCoordInfo _ = False

checkIfCorrectCoordInfo' :: Document -> Bool
checkIfCorrectCoordInfo' (DMap((s1, d1) : (s2, d2) : _))
  | s2 == "tail" && d2 == DNull = True
  | s1 /= "head" && s2 == "tail" = False
  | s1 == "head" && s2 /= "tail" = False
  | s1 == "head" && s2 == "tail" && checkIfCorrectCoordInfo'' d1 = checkIfCorrectCoordInfo' d2 
  | otherwise = False
checkIfCorrectCoordInfo' (DMap((s1, d1) : _))
 | s1 == "tail" && d1 == DNull = True
checkIfCorrectCoordInfo' _ = False

checkIfCorrectCoordInfo'' :: Document -> Bool
checkIfCorrectCoordInfo'' (DMap ((s1, d1) : (s2, d2) : _))
  | s1 == "col" && s2 == "row" = checkIfCorrectCoordInfo''' d1 && checkIfCorrectCoordInfo''' d2
  | otherwise = False
checkIfCorrectCoordInfo'' _ = False

checkIfCorrectCoordInfo''' :: Document -> Bool
checkIfCorrectCoordInfo''' (DInteger _) = True
checkIfCorrectCoordInfo''' _ = False


-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State l) doc
  | not (existsStateInfo doc "game_setup_id") = Left "Game setup Id not found."
  | not (existsStateInfo doc "occupied_cols") = Left "Occupied collumns information is missing."
  | not (existsStateInfo doc "occupied_rows") = Left "Occupied rows information is missing."
  | not (existsStateInfo doc "number_of_hints") = Left "Hint information is missing."
  | otherwise = Right $ gameStartRecursive (State l) doc

gameStartRecursive :: State -> Document -> State
gameStartRecursive (State l) (DMap ((s, d) : xs)) =
  case s of
    "game_setup_id" -> State (("toggles", DString []) : ("hints", DString []) : ((s, d) : l))
    "occupied_rows" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "occupied_cols" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "number_of_hints" -> gameStartRecursive (State ((s, d) : l)) (DMap xs)
    _ -> gameStartRecursive (State l) (DMap xs)
gameStartRecursive _ _ = emptyState

existsStateInfo :: Document -> String -> Bool
existsStateInfo (DMap ((s, _) : xs)) str =
  s == str || existsStateInfo (DMap xs) str
existsStateInfo _ _ = False

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


parseDoc :: Document -> String -> String

parseDoc DNull acc = acc ++ "null"

parseDoc (DInteger d) acc = acc ++ show d

parseDoc (DString d) acc = acc ++ d

parseDoc (DList (x : xs)) acc =
  parseDoc' (DList xs) (parseDoc x (acc ++ "["))

parseDoc (DList []) acc =
  acc ++ "[]"

parseDoc (DMap((str, val) : xs)) acc =
  parseDoc' (DMap xs) (parseDoc val (acc ++ "{" ++ str ++ ": "))

parseDoc (DMap[]) acc = acc ++ "{}"

--inner functions for parsing----

parseDoc' :: Document -> String -> String

parseDoc' DNull acc = acc ++ "null"

parseDoc' (DInteger d) acc = acc ++ show d

parseDoc' (DString d) acc = acc ++ d

parseDoc' (DList (x : xs)) acc =
  parseDoc' (DList xs) (parseDoc x (acc ++ ", ") )

parseDoc' (DList []) acc =
  acc ++ "]"

parseDoc' (DMap((str, val) : xs)) acc =
  parseDoc' (DMap xs) (parseDoc val (acc ++ ", " ++ str ++ ": "))

parseDoc' (DMap[]) acc = acc ++ "}"