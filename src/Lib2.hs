{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
hint (State ((st, doc) : xs)) d
  | State ((st, doc) : xs) == emptyState = Left "Game is not started"
  | doc == DNull = Left "Hint passed from server is empty"
  | getAvailableHintNum <= 0 = Left "No hints available"
  | otherwise = Right $ hints (State ((st, doc) : xs)) d

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

getAvailableHintNum :: State -> Int
getAvailableHintNum (State ((st, doc) : xs))
  | st == "number_of_hints" = charToInt $ head $ getDIntValue (show doc) [] []
  | otherwise = getAvailableHintNum (State xs)
getAvailableHintNum _ = 0

--See if document is correct and return true bool and return left string if false



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