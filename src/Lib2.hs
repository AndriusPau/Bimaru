{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document(..), Check(..), Coord(..))
import Lib1 (State(..))

-- IMPLEMENT
-- First, make Check an instance of ToDocument class

-- Current plans on the Check structure after running it through ToDocument
-- "DMap[(String  , DList[DMap[(String, DInteger),  (String, DInteger)], DMap[(String, DInteger), (String, DInteger)], (...) ]  )]"
-- "    [("coords",      [    [("col" , 0       ),  ("row",  2       )], (...) ]  )]"

instance ToDocument Check where
    toDocument (Check t) = toDocumentRecursive t (DMap [("coords", DList [])])

toDocumentRecursive :: [Coord] -> Document -> Document
toDocumentRecursive (((Coord c r)) : xs) (DMap [(str, DList l)]) =
  toDocumentRecursive xs (DMap [(str, DList (l ++ [DMap [("col", DInteger c), ("row", DInteger r)  ]  ]  )  )]  )
--toDocumentRecursive [coord] (DMap [(_, DList[])]) = toDocumentRecursive
toDocumentRecursive _ doc = doc

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
{-
  "---\n" ++ 
  "coords:\n" ++ 
  
  "- col: 0\n" ++ 
  "  row: 2\n" ++ 

  "- col: 0\n" ++ 
  "  row: 5\n" ++

  "- col: 2\n" ++ 
  "  row: 3\n" ++

  "- col: 3\n" ++ 
  "  row: 3\n" ++

  "- col: 4\n" ++ 
  "  row: 5\n" ++

  "- col: 2\n" ++ 
  "  row: 7\n" ++

  "- col: 3\n" ++ 
  "  row: 7\n" ++

  "- col: 4\n" ++ 
  "  row: 7\n" ++

  "- col: 6\n" ++ 
  "  row: 2\n" ++

  "- col: 6\n" ++ 
  "  row: 3\n" ++

  "- col: 6\n" ++ 
  "  row: 5\n" ++

  "- col: 6\n" ++ 
  "  row: 6\n" ++

  "- col: 6\n" ++ 
  "  row: 7\n" ++
  
  "- col: 6\n" ++ 
  "  row: 8\n" ++

  "- col: 8\n" ++ 
  "  row: 0\n" ++

  "- col: 8\n" ++ 
  "  row: 1\n" ++

  "- col: 8\n" ++ 
  "  row: 8\n" ++

  "- col: 9\n" ++ 
  "  row: 4\n" ++

  "- col: 9\n" ++ 
  "  row: 5\n" ++

  "- col: 9\n" ++ 
  "  row: 6\n"
-}



-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument doc = parseDoc doc []

--(DMap[("coords", DList[DMap[("col", DInteger 0),("row", DInteger 2)], DMap[("col", DInteger 0), ("row", DInteger 5)], DMap[("col", DInteger 2), ("row", DInteger 3)], DMap[("col", DInteger 3), ("row", DInteger 3)], DMap[("col", DInteger 4), ("row", DInteger 5)], DMap[("col", DInteger 2), ("row", DInteger 7)], DMap[("col", DInteger 3), ("row", DInteger 7)], DMap[("col", DInteger 4), ("row", DInteger 7)], DMap[("col", DInteger 6), ("row", DInteger 2)], DMap[("col", DInteger 6), ("row", DInteger 3)], DMap[("col", DInteger 6), ("row", DInteger 5)], DMap[("col", DInteger 6), ("row", DInteger 6)], DMap[("col", DInteger 6), ("row", DInteger 7)], DMap[("col", DInteger 6), ("row", DInteger 8)], DMap[("col", DInteger 8), ("row", DInteger 0)], DMap[("col", DInteger 8), ("row", DInteger 1)], DMap[("col", DInteger 8), ("row", DInteger 8)], DMap[("col", DInteger 9), ("row", DInteger 4)], DMap[("col", DInteger 9), ("row", DInteger 5)], DMap[("col", DInteger 9), ("row", DInteger 6)]])]) ""





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
gameStart (State l) doc
  | doc /= DNull = Right $ gameStartRecursive (State l) doc
  | otherwise = Left "test"

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