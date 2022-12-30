{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
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
    Right (f a, rest')

instance Alternative Parser where
  empty = Parser $ const $ Left "empty"
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Left _ -> p2 input
      x      -> x


parseDocument :: String -> Either String Document
parseDocument yaml = 
  do
    case runParser (parseDocument' 0) yaml of
      Left err -> Left err
      Right (doc, rest) -> if null rest then Right doc else Left $ "parseDocument: not all input consumed; rest: " ++ rest

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

parseChar :: Char -> Parser Char
parseChar c = Parser $ \case
  (x : xs)
    | x == c -> Right (c, xs)
    | otherwise
    -> Left $ "Expected '" ++ [c] ++ "'," ++ " but got '" ++ [x] ++ "'"
  [] -> Left $ "Expected '" ++ [c] ++ "', but got empty string"

parseStr :: String -> Parser String
parseStr str = Parser $ \input ->
  let result = runParser (traverse parseChar str) input in
  case result of
    Left _ -> Left $ "Expected \"" ++ str ++ "\", but got \"" ++ input ++ "\""
    _ -> result

spanParserMany :: (Char -> Bool) -> Parser String
spanParserMany f = many $ parseIf f

spanParserSome :: (Char -> Bool) -> Parser String
spanParserSome f = some $ parseIf f

parseIf :: (Char -> Bool) -> Parser Char
parseIf f =
  Parser $ \case
  y : ys
    | f y -> Right (y, ys)
    | otherwise
    -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
  [] -> Left "Expected a character, but reached end of string"

parseIf' :: (Char -> Bool) -> Parser Char
parseIf' f =
  Parser $ \case
  y : ys
    | f y -> Right (' ', y : ys)
    | otherwise
    -> Left $ "Expected a character, but got '" ++ [y] ++ "'"
  [] -> Left "Expected a character, but reached end of string"

intLiteral :: Parser Int
intLiteral = parseChar '-' *> fmap negate nonNegativeLiteral <|> nonNegativeLiteral

nonNegativeLiteral :: Parser Int
nonNegativeLiteral = read <$> spanParserSome isDigit

stringLiteral :: Parser String
stringLiteral = spanParserSome isAlphaNumSpace

parseDNull :: Parser Document
parseDNull = Parser $ \input -> do
  let result = runParser (parseStr "null\n") input in
    case result of
      Left a -> Left a
      _ -> Right (DNull, drop 5 input)

parseDInteger :: Parser Document
parseDInteger = DInteger <$> (optional (spanParserMany isSpace) *> intLiteral <* parseChar '\n')

parseDString :: Parser Document
parseDString = DString <$> (optional (spanParserMany isSpace) *> optional (parseChar '"') *> optional (parseChar '\'') *> stringLiteral <* optional (parseChar '\'') <* optional (parseChar '"') <* parseChar '\n')

parseDMap :: Int -> Parser Document
parseDMap prevIndent = Parser $ \input ->
  let indentation = (countSpaces input `div` 2) in
  if prevIndent == indentation then runParser ( DMap <$> some (parsePair prevIndent)) input
    else if prevIndent < indentation then runParser (parseDocument' prevIndent) input
    else Left "errorDMAP"

parsePair :: Int -> Parser (String, Document)
parsePair prevIndent = Parser $ \input ->
  let indentation = (countSpaces input `div` 2) in
    if prevIndent == indentation then runParser ((,) <$> (optional (parseChar '\n') *> removeSpaces prevIndent *> optional (parseChar '"') *> optional (parseChar '\'') *> stringLiteral <* optional (parseChar '\'') <* optional (parseChar '"')) <*> (parseChar ':' *> optional(parseChar ' ') *> optional (parseChar '\n') *> parseDocumentInDMap prevIndent)) input
    else Left "errorDPAIR"

parseDList :: Int -> Parser Document
parseDList prevIndent = Parser $ \input ->
  let indentation = countSpaces input `div` 2 in
    if prevIndent == indentation then runParser (DList <$> some ( optional (parseChar '\n') *> removeSpaces prevIndent *> parseStr "- " *> addSpaces (prevIndent+1) *> parseListItem prevIndent)) input
    else Left "errorDLIST"

parseListItem :: Int -> Parser Document
parseListItem prevIndent = Parser $ \input ->
  let indentation = countSpaces input `div` 2 - 1 in
    if prevIndent == indentation then runParser ( optional (parseChar '\n') *> parseDocumentInList prevIndent) input
    else Left "errorDLISTITEM"

parseEmptyDList :: Parser Document
parseEmptyDList = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "[]" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DList [], drop 3 (dropWhile isSpace input))

parseEmptyDMap :: Parser Document
parseEmptyDMap = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "{}" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DMap [], drop 3 (dropWhile isSpace input))

parseEmptyDStringDoubleQuotes :: Parser Document
parseEmptyDStringDoubleQuotes = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "\"\"" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DString "", drop 3 (dropWhile isSpace input))

parseEmptyDStringSingleQuotes :: Parser Document
parseEmptyDStringSingleQuotes = Parser $ \input ->
  let result = runParser (spanParserMany isSpace *> parseStr "''" <* parseChar '\n') input in
    case result of
      Left a -> Left a
      _ -> Right (DString "", drop 3 (dropWhile isSpace input))

parseDocument' :: Int -> Parser Document
parseDocument' prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList  prevIndent <|> parseDMap prevIndent  <|> parseDString

parseDocumentInList :: Int -> Parser Document
parseDocumentInList prevIndent = parseDInteger <|> parseDNull <|> parseEmptyDStringDoubleQuotes <|> parseEmptyDStringSingleQuotes <|> parseEmptyDMap <|> parseEmptyDList <|> parseDList (prevIndent+1) <|> parseDMap (prevIndent+1) <|> parseDString

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
