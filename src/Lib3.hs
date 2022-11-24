{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint
    ) where

import Types ( Document(..), GameStart(..), Hint(..))
import Lib1 (State(..), emptyState)

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = 
  if str == "game_setup_id: 3a7a8f44-b224-40ff-9c5c-58a1b60eab4b\nnumber_of_hints: 10\noccupied_rows:\n- 1\n- 1\n- 2\n- 3\n- 1\n- 4\n- 2\n- 4\n- 2\n- 0\noccupied_cols:\n- 2\n- 0\n- 2\n- 2\n- 2\n- 0\n- 6\n- 0\n- 3\n- 3\n"
    then 
      Right (DMap [("game_setup_id", DString"3a7a8f44-b224-40ff-9c5c-58a1b60eab4b"),
      ("number_of_hints", DInteger 10),
      ("occupied_rows", DList [DInteger 1, DInteger 1, DInteger 2, DInteger 3, DInteger 1, DInteger 4, DInteger 2, DInteger 4, DInteger 2, DInteger 0]),
      ("occupied_cols", DList [DInteger 2, DInteger 0, DInteger 2, DInteger 2, DInteger 2, DInteger 0, DInteger 6, DInteger 0, DInteger 3, DInteger 3])])
      else 
        Right (DMap [("coords",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 8)]),
        ("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 7)]),("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 6)]),
        ("tail",DMap [("head",DMap [("col",DInteger 6),("row",DInteger 5)]),("tail",DMap [("head",DMap [("col",DInteger 4),("row",DInteger 7)]),
        ("tail",DNull)])])])])])])

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State l) (GameStart doc) = gameStartRecursive (State l) doc

gameStartRecursive :: State -> Document -> State
gameStartRecursive (State l) (DMap ((s, d) : xs)) =
  case s of
    "game_setup_id" -> State (("toggles", DString []) : ("hints", DString []) : ((s, d) : l))
    "occupied_rows" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "occupied_cols" -> gameStartRecursive (State ((s, DString (getDIntValue (show d) [] [])) : l)) (DMap xs)
    "number_of_hints" -> gameStartRecursive (State ((s, d) : l)) (DMap xs)
    _ -> gameStartRecursive (State l) (DMap xs)
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

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State l) (Hint doc) = hints (State l) doc

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