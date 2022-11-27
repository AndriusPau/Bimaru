{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint
    ) where

import Types ( Document(..), GameStart(..), Hint(..))
import Lib1 (State(..), emptyState)
import Control.Applicative
import Data.Char


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
    Right $ (f a, rest')

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
parseDocument yaml = do
    case runParser (parseDocument' 0) yaml of
      Left err -> Left err
      Right (doc, rest) -> if null rest then Right doc else Left $ "parseDocument: not all input consumed; rest: " ++ rest

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

--This parses a single character from the input string
--Returns Right Parser Char on successful parse holding the parsed character and the rest of the input: String -> (Char, String)
--Returns Left String with error message on failure
parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
  case input of
    (x : xs) | x == c -> Right (c, xs)
             | otherwise -> Left $ "Expected '" ++ [c] ++ "'," ++ " but got '" ++ [x] ++ "'"
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
  Parser $ \input ->
    case input of
      y:ys
        | f y       -> Right (y, ys)
        | otherwise -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
      [] -> Left $ "Expected a character, but reached end of string"

--This parses a single character from the input string if the given predicate is true without removing the character from the rest of the input
--Returns Right Parser Char on successful parse holding placeholder character and input: String -> (Char, String)
--Returns Left String with error message on failure
parseIf' :: (Char -> Bool) -> Parser Char
parseIf' f =
  Parser $ \input ->
    case input of
      y:ys
        | f y       -> Right (' ', y:ys)
        | otherwise -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
      [] -> Left $ "Expected a character, but reached end of string"

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
isAlphaNumSpace c = isAlphaNum c || c == ' '

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
