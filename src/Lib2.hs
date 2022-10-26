module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document(..), Check )
import Lib1 (State(..))

-- IMPLEMENT
-- First, make Check an instance of ToDocument class

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument doc = parseDoc (DMap[("coords", DList[DMap[("col", DInteger 0),("row", DInteger 2)], DMap[("col", DInteger 0), ("row", DInteger 5)], DMap[("col", DInteger 2), ("row", DInteger 3)], DMap[("col", DInteger 3), ("row", DInteger 3)], DMap[("col", DInteger 4), ("row", DInteger 5)], DMap[("col", DInteger 2), ("row", DInteger 7)], DMap[("col", DInteger 3), ("row", DInteger 7)], DMap[("col", DInteger 4), ("row", DInteger 7)], DMap[("col", DInteger 6), ("row", DInteger 2)], DMap[("col", DInteger 6), ("row", DInteger 3)], DMap[("col", DInteger 6), ("row", DInteger 5)], DMap[("col", DInteger 6), ("row", DInteger 6)], DMap[("col", DInteger 6), ("row", DInteger 7)], DMap[("col", DInteger 6), ("row", DInteger 8)], DMap[("col", DInteger 8), ("row", DInteger 0)], DMap[("col", DInteger 8), ("row", DInteger 1)], DMap[("col", DInteger 8), ("row", DInteger 8)], DMap[("col", DInteger 9), ("row", DInteger 4)], DMap[("col", DInteger 9), ("row", DInteger 5)], DMap[("col", DInteger 9), ("row", DInteger 6)]])]) ""





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
valid check data for testing: 
DMap[("coords", DList[DMap[("col", DInteger 0),("row", DInteger 2)], DMap[("col", DInteger 0), ("row", DInteger 5)], DMap[("col", DInteger 2), ("row", DInteger 3)], DMap[("col", DInteger 3), ("row", DInteger 3)], DMap[("col", DInteger 4), ("row", DInteger 5)], DMap[("col", DInteger 2), ("row", DInteger 7)], DMap[("col", DInteger 3), ("row", DInteger 7)], DMap[("col", DInteger 4), ("row", DInteger 7)], DMap[("col", DInteger 6), ("row", DInteger 2)], DMap[("col", DInteger 6), ("row", DInteger 3)], DMap[("col", DInteger 6), ("row", DInteger 5)], DMap[("col", DInteger 6), ("row", DInteger 6)], DMap[("col", DInteger 6), ("row", DInteger 7)], DMap[("col", DInteger 6), ("row", DInteger 8)], DMap[("col", DInteger 8), ("row", DInteger 0)], DMap[("col", DInteger 8), ("row", DInteger 1)], DMap[("col", DInteger 8), ("row", DInteger 8)], DMap[("col", DInteger 9), ("row", DInteger 4)], DMap[("col", DInteger 9), ("row", DInteger 5)], DMap[("col", DInteger 9), ("row", DInteger 6)]])]
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