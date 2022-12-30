{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
module Lib4(emptyState, State, gameStart, GameStart, parseDocument, render, renderDocument
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


newtype Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    return (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, rest) <- p1 input
    (a, rest') <- p2 rest
    Right (f a, rest')

instance Alternative Parser where
  empty = Parser $ const $ Left "empty"
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Left _ -> p2 input
      x      -> x


-- This parses a string of YAML format
-- Returns Right Document on successful parse
-- Returns Left String with error message on failure
parseDocument :: String -> Either String Document
-- parseDocument = Left . show
parseDocument yaml =
  do
    case runParser (parseDocument' 0) yaml of
      Left err -> Left err
      Right (doc, rest) -> if null rest then Right doc else Left $ "parseDocument: not all input consumed; rest: " ++ rest

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

--This parses a single character from the input string
--Returns Right Parser Char on successful parse holding the parsed character and the rest of the input: String -> (Char, String)
--Returns Left String with error message on failure
parseChar :: Char -> Parser Char
parseChar c = Parser $ \case
  (x : xs)
    | x == c -> Right (c, xs)
    | otherwise
    -> Left $ "Expected '" ++ [c] ++ "'," ++ " but got '" ++ [x] ++ "'"
  [] -> Left $ "Expected '" ++ [c] ++ "', but got empty string"

--This parses a string of characters from the input string
--Returns Right Parser String on successful parse holding the parsed string and the rest of the input: String -> (String, String)
--Returns Left String with error message on failure
parseStr :: String -> Parser String
parseStr str = Parser $ \input ->
  let result = runParser (traverse parseChar str) input in
  case result of
    Left _ -> Left $ "Expected \"" ++ str ++ "\", but got \"" ++ input ++ "\""
    _ -> result

--This parses many (0 or more) characters from the input string while the given predicate is true
--Returns Right Parser String on successful parse holding the parsed string and the rest of the input: String -> (String, String)
--Returns Left String with error message on failure
spanParserMany :: (Char -> Bool) -> Parser String
spanParserMany f = many $ parseIf f

--This parses some (1 or more) characters from the input string while the given predicate is true
--Returns Right Parser String on successful parse holding the parsed string and the rest of the input: String -> (String, String)
--Returns Left String with error message on failure
spanParserSome :: (Char -> Bool) -> Parser String
spanParserSome f = some $ parseIf f

--This parses a single character from the input string if the given predicate is true
--Returns Right Parser Char on successful parse holding the parsed character and the rest of the input: String -> (Char, String)
--Returns Left String with error message on failure
parseIf :: (Char -> Bool) -> Parser Char
parseIf f =
  Parser $ \case
  y : ys
    | f y -> Right (y, ys)
    | otherwise
    -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
  [] -> Left "Expected a character, but reached end of string"

--This parses a single character from the input string if the given predicate is true without removing the character from the rest of the input
--Returns Right Parser Char on successful parse holding placeholder character and input: String -> (Char, String)
--Returns Left String with error message on failure
parseIf' :: (Char -> Bool) -> Parser Char
parseIf' f =
  Parser $ \case
  y : ys
    | f y -> Right (' ', y : ys)
    | otherwise
    -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
  [] -> Left "Expected a character, but reached end of string"

--This parses a dash indicating a negative number and digits or digits if the number is positive
--Returns Right Parser Int on successful parse holding the parsed integer and the rest of the input: String -> (Int, String)
--Returns Left String with error message on failure
intLiteral :: Parser Int
intLiteral = parseChar '-' *> fmap negate nonNegativeLiteral <|> nonNegativeLiteral

--This parses and a non negative integer
--Returns Right Parser Int on successful parse holding the parsed integer and the rest of the input: String -> (Int, String)
--Returns Left String with error message on failure
nonNegativeLiteral :: Parser Int
nonNegativeLiteral = read <$> spanParserSome isDigit

--This parses all alphabetic characters, digits and spaces
--Returns Right Parser String on successful parse holding the parsed string and the rest of the input: String -> (String, String)
--Returns Left String with error message on failure
stringLiteral :: Parser String
stringLiteral = spanParserSome isAlphaNumSpace

--This parses a DNull value if the input string starts with "null\n"
--Returns Right Parser Document on successful parse holding the parsed DNull value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseDNull :: Parser Document
parseDNull = Parser $ \input -> do
  let result = runParser (parseStr "null\n") input in
    case result of
      Left a -> Left a
      _ -> Right (DNull, drop 5 input)

--This parses a DInteger value if the input string starts with any spaces followed by an intLiteral
--Returns Right Parser Document on successful parse holding the parsed DInteger value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseDInteger :: Parser Document
parseDInteger = DInteger <$> (optional (spanParserMany isSpace) *> intLiteral <* parseChar '\n')

--This parses a DString value if the input string starts with any spaces followed by single or double quotes and a stringLiteral and ending on quotes
--Returns Right Parser Document on successful parse holding the parsed DString value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseDString :: Parser Document
parseDString = DString <$> (optional (spanParserMany isSpace) *> optional (parseChar '"') *> optional (parseChar '\'') *> stringLiteral <* optional (parseChar '\'') <* optional (parseChar '"') <* parseChar '\n')

--This parses a DMap value recursively if the input string starts with spaces equal to passed Int * 2
--Returns Right Parser Document on successful parse holding the parsed DMap value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseDMap :: Int -> Parser Document
parseDMap prevIndent = Parser $ \input ->
  let indentation = (countSpaces input `div` 2) in
  if prevIndent == indentation then runParser ( DMap <$> some (parsePair prevIndent)) input
    else if prevIndent < indentation then runParser (parseDocument' prevIndent) input
    else Left "errorDMAP"

--This parses a DMap value recursively if the input string starts with spaces equal to passed Int * 2, DMap key followed by a Document type
--Returns Right Parser (String, Document) on successful parse holding the parsed DMap key and value as a touple and the rest of the input: String -> ((String, Document), String)
--Returns Left String with error message on failure
parsePair :: Int -> Parser (String, Document)
parsePair prevIndent = Parser $ \input ->
  let indentation = (countSpaces input `div` 2) in
    if prevIndent == indentation then runParser ((,) <$> (optional (parseChar '\n') *> removeSpaces prevIndent *> optional (parseChar '"') *> optional (parseChar '\'') *> stringLiteral <* optional (parseChar '\'') <* optional (parseChar '"')) <*> (parseChar ':' *> optional(parseChar ' ') *> optional (parseChar '\n') *> parseDocumentInDMap prevIndent)) input
    else Left "errorDPAIR"

--This parses a DList value recursively if the input string starts with spaces equal to passed Int * 2 followed by "- "
--Returns Right Parser Document on successful parse holding the parsed DList value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseDList :: Int -> Parser Document
parseDList prevIndent = Parser $ \input ->
  let indentation = countSpaces input `div` 2 in
    if prevIndent == indentation then runParser (DList <$> some ( optional (parseChar '\n') *> removeSpaces prevIndent *> parseStr "- " *> addSpaces (prevIndent+1) *> parseListItem prevIndent)) input
    else Left "errorDLIST"

--This parses a DList value recursively if the input string starts with spaces equal to passed Int * 2
--Returns Right Parser Document on successful parse holding the parsed DList value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseListItem :: Int -> Parser Document
parseListItem prevIndent = Parser $ \input ->
  let indentation = countSpaces input `div` 2 - 1 in
    if prevIndent == indentation then runParser ( optional (parseChar '\n') *> parseDocumentInList prevIndent) input
    else Left "errorDLISTITEM"

--This parses an empty DList if the input string starts with n spaces followed by "[]\n"
--Returns Right Parser Document on successful parse holding an empty DList and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseEmptyDList :: Parser Document
parseEmptyDList = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "[]" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DList [], drop 3 (dropWhile isSpace input))

--This parses an empty DMap if the input string starts with n spaces followed by "{}\n"
--Returns Right Parser Document on successful parse holding an empty DMap and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure 
parseEmptyDMap :: Parser Document
parseEmptyDMap = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "{}" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DMap [], drop 3 (dropWhile isSpace input))

--This parses an empty DString if the input string starts with n spaces followed by "\"\"\n"
--Returns Right Parser Document on successful parse holding an empty DString and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseEmptyDStringDoubleQuotes :: Parser Document
parseEmptyDStringDoubleQuotes = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "\"\"" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DString "", drop 3 (dropWhile isSpace input))

--This parses an empty DString if the input string starts with n spaces followed by "''\n"
--Returns Right Parser Document on successful parse holding an empty DString and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseEmptyDStringSingleQuotes :: Parser Document
parseEmptyDStringSingleQuotes = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "''" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DString "", drop 3 (dropWhile isSpace input))

--This recursively parses a document if the input string starts with n spaces followed by a Document type value
--Returns Right Parser Document on successful parse holding the parsed Document value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseDocument' :: Int -> Parser Document
parseDocument' prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList  prevIndent <|> parseDMap prevIndent  <|> parseDString

--This recursively parses a document if the input string starts with n spaces followed by a Document type value
--Returns Right Parser Document on successful parse holding the parsed Document value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseDocumentInList :: Int -> Parser Document
parseDocumentInList prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList (prevIndent+1) <|> parseDMap (prevIndent+1) <|> parseDString

--This recursively parses a document if the input string starts with n spaces followed by a Document type value
--Returns Right Parser Document on successful parse holding the parsed Document value and the rest of the input: String -> (Document, String)
--Returns Left String with error message on failure
parseDocumentInDMap :: Int -> Parser Document
parseDocumentInDMap prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList prevIndent <|> parseDMap (prevIndent+1) <|> parseDString

-- HELPER FUNCTIONS

--This checks if the given character is an alphabetic character, a digit or a space
isAlphaNumSpace :: Char -> Bool
isAlphaNumSpace c = isAlphaNum c || c == ' ' || c == '_' || c == '-'
--This removes n spaces from the beginning of the input string
--Returns Right Parser String the removed spaces and the rest of the input string: String -> (String, String)
--Returns Left String with error message on failure
removeSpaces :: Int -> Parser String
removeSpaces n = Parser $ \input ->
  let indentation = countSpaces input `div` 2 in
    if n <= indentation then Right ("", drop (n*2) input)
    else Left "error"

--This adds n spaces to the beginning of the input string
--Returns Right Parser String holding an empty character and spaces with input string: String -> (String, String)
--Returns Left String with error message on failure
addSpaces :: Int -> Parser String
addSpaces n = Parser $ \input -> addSpace input n

--This recursively adds two spaces to the beginning of the input string
--Returns Right (String, String) holding an empty character and spaces with input string
--Returns Left String with error message on failure
addSpace :: String -> Int -> Either String (String, String)
addSpace acc 0 = Right ("", acc)
addSpace acc n = addSpace ("  " ++ acc) (n-1)

--This counts the number of spaces at the beginning of the input string
--Returns the number of spaces
countSpaces :: String -> Int
countSpaces str =
  let (a, _) = span isSpaceOrNewLine str in
    if null a
      then 0
    else
      if head a == '\n'
        then length a - 1
      else length a

--This checks if the given character is a space or a new line
isSpaceOrNewLine :: Char -> Bool
isSpaceOrNewLine c = isSpace c || c == '\n'

---------------------------------------------------------------------------------------------------
-- Render implementation

-- IMPLEMENT
-- Renders your game board
-- Render gets the current gamestate and turns it to a string to be printed to the screen.
-- Uses drawGridLineNum, drawGridTop, drawGrid functions.
-- State - The current gamestate.
-- String - The result string that has the whole gameboard and other information for displayment.
render :: State -> String
-- render st = "      " ++ drawGridLineNum ++ "\n ┌────────────────────────\n │    " ++ drawGridTop st ++ "\n │\n" ++ drawGrid st [] 0

render = show

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