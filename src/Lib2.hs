module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document(..), Check )
import Lib1 (State(..))

-- IMPLEMENT
-- First, make Check an instance of ToDocument class

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument (DString d) = error d





-- gameStart :: State -> Document -> State
-- gameStart (State l) (DMap ((s, d) : xs)) =
--   case s of
--     "game_setup_id" -> State (("toggles", DString []) : ("hints", DString []) : ((s, d) : l))
--     "occupied_rows" -> gameStart (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
--     "occupied_cols" -> gameStart (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
--     _ -> gameStart (State ((s, d) : l)) (DMap xs)
-- gameStart _ _ = emptyState

-- This is very initial state of your program
emptyState :: State
emptyState = State []


-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
--hint (State l) h = Right $ State $ ("Hint " ++ show h) : l
hint s d = Right s


-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State l) doc = Right $ gameStartRecursive (State l) doc

gameStartRecursive :: State -> Document -> State
gameStartRecursive (State l) (DMap ((s, d) : xs)) =
  case s of
    "game_setup_id" -> State (("toggles", DString []) : ("hints", DString []) : ((s, d) : l))
    "occupied_rows" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "occupied_cols" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    _ -> gameStartRecursive (State ((s, d) : l)) (DMap xs)
gameStartRecursive _ _ = emptyState

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





{-
-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint (State l) doc = Right $ ("Hint " ++ show doc) : l
-}

--  Danielius working area!!
{-
addFirstLine :: String -> String
addFirstLine x = "---\n" ++ x

checkIfDNull :: String -> Bool  --sugalvoti kaip rasti DNull pabaiga. Neaišku kaip veikia
checkIfDNull (x:xs) =
    if length (x: xs) >= 5
        then (take 5 (x: xs)) == "DNull"
    else False

checkIfDInteger :: String -> Bool

checkIfDInteger str =
  if length str >= 9
    then take 9 str == "DInteger "
  else False

checkIfDString :: String -> Bool

checkIfDString str =
  if length str >= 8
    then take 8 str == "DString "
  else False


-- parsers goes down from here

parseDNull :: Document -> String

parseDNull doc =
  if checkIfDNull (show doc)
    then "null"
    else "!error: Not a type of DNull!" -- reikia su Either padaryt

parseDInteger :: Document -> String

parseDInteger doc = 
  if checkIfDInteger (show doc)
    then drop 9 (show doc)
  else "!error: Not a type of DInteger!"

parseDString :: String -> String-> String

parseDString str rez=
  if checkIfDString (str)
    then parDString (drop 9 (str)) rez
  else "!error: Not a type of DString!"

parDString :: String -> String -> String

parDString (x : xs) rez =
  if x == '"'
    then rez
  else parDString xs (rez ++ [x])

parDString [] str = str

deleteString :: String -> String -- deletes the left side of the (") found in string (included ("))

deleteString (x : xs) =
  if x == '"'
    then xs
  else deleteString xs

deleteString [] = ""

-}

parseDoc :: Document -> String -> String

parseDoc DNull acc = acc ++ "null"

parseDoc (DInteger d) acc = acc ++ show d

parseDoc (DString d) acc = acc ++ d

parseDoc (DList (x : xs)) acc =
  parseDoc' (DList xs) (parseDoc x (acc ++ "["))

parseDoc (DList []) acc = 
  acc ++ "]"

parseDoc (DMap((str, val) : xs)) acc = 
  parseDoc' (DMap xs) (parseDoc val (acc ++ "{" ++ str ++ ": "))

parseDoc (DMap[]) acc = acc ++ "}"

--inner functions for parsing--

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